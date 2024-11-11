||| Driver for talking to 510 Scale over USB.
|||
||| The scale sends a 6-byte packet, which must be manually swizzled
||| to decode the weight.
module USBScale


import Barcode
import Container
import Control.ANSI
import Data.Bits
import Data.Buffer
import Data.Either
import Data.Vect
import Derive.Prelude
import JSON.Derive
import Measures
import System.Concurrency
import System
import System.File
import TUI
import TUI.Component
import TUI.MainLoop
import TUI.MainLoop.InputShim
import Util


%language ElabReflection
%default total


||| Move to Utils lib
debug : Show a => a -> IO Builtin.Unit
debug value = do
  _ <- fPutStrLn stderr (show value)
  pure ()

||| Unit to use when we can't determine from the USB traffic
defaultUnit : UnitT Mass
defaultUnit = Grams

||| Convert integer unit into type
units : Bits8 -> UnitT Mass
units 1  = MilliGrams
units 2  = Grams
units 3  = KiloGrams
units 11 = Ounces
units 12 = Pounds
units _  = defaultUnit

||| Result of scale IO operation
public export
data Result
  = Empty
  | Weighing
  | Fault String
  | Ok Weight
%runElab derive "Result" [Show,Ord,Eq,FromJSON,ToJSON]

||| Calculate the current weight from the raw binary values
calcWeight : Bits8 -> Int -> Int -> Int -> Weight
calcWeight unit msb lsb exponent =
  let
    mantissa = cast {to = Double} ((msb `shiftL` 8) .|. lsb)
    unit   = units unit
    scaled = case unit of
      Ounces => mantissa / 10.0
      _      => mantissa
  in case exponent of
      0x00 => Q (cast scaled) unit
      0xff => Q (cast scaled) unit
      _    => Q (pow scaled (cast exponent)) unit

||| Decode the 6-byte HID packet into a scale value
decode : List Bits8 -> Result
decode [report, status, unit, exp, lsb, msb] =
  if report == 0x03
    then case status of
      0x01 => Fault "Fault"
      0x02 => Empty
      0x03 => Weighing
      0x04 => Ok $ calcWeight unit (cast msb) (cast lsb) (cast exp)
      0x05 => Fault "Negative Weight"
      0x06 => Fault "Overweight"
      0x07 => Fault "Recalibrate"
      0x08 => Fault "Rezero"
      _    => Fault $ "Unknown status code: " ++ show status
    else Fault "Error Reading Scale!"
decode _ = Fault"Error, invalid packet"

||| Synchronously read from the scale
|||
||| We can't necessarily trust the first weight we get, so we wait for
||| a few valid weights to come in before returning the value.
export partial
getWeight : String -> IO (Either String Weight)
getWeight path = do
  Just buf <- newBuffer 6 | Nothing => pure $ Left "Couldn't allocate buffer"
  withFile path Read onError (loopN buf discardCount Nothing)
  where
    discardCount : Nat
    discardCount = 5

    loopN : Buffer -> Nat -> Maybe Weight -> File -> IO (Either String Weight)
    loopN buf Z     r file = pure $ maybeToEither "impossible" r
    loopN buf (S n) r file = do
      putStrLn "\{show r}: (\{show n} tries remaining...)"
      _ <- readBufferData file buf 0 6
      d <- bufferData' buf
      case decode d of
        Ok w => loopN buf n     (Just w) file
        _    => loopN buf (S n) r        file

    onError : FileError -> IO String
    onError err = pure $ show err

||| Continuously read from the scale into a channel.
partial
run : (USBScale.Result -> IO Builtin.Unit) -> String -> IO Builtin.Unit
run post path = do
  Just buf <- newBuffer 6
    | Nothing => debug (the String "error, could not allocate buffer")
  Right _ <- withFile path Read onError (loop post buf)
    | Left err => debug err
  pure ()
  where
    loop
      : (Result -> IO Builtin.Unit)
      -> Buffer
      -> File
      -> IO (Either String Builtin.Unit)
    loop post buf file = do
      _ <- readBufferData file buf 0 6
      d <- bufferData' buf
      post (decode d)
      loop post buf file

    onError : FileError -> IO String
    onError err = pure $ show err

||| Spawn the scale reading thread
|||
||| The function parameter should post the USBScale.result to the main
||| event queue.
export partial
spawn : String -> (Result -> IO Builtin.Unit) -> IO ThreadID
spawn path post = fork (run post path)


namespace SmartScale
  ||| An MVP of my "Smart Scale" concept
  |||
  ||| Basically: we have a list of containers, a web server that
  ||| allows for image uploads from a mobile phone, and a digital
  ||| scale.
  |||
  ||| We can tear each container independently.
  |||
  ||| We can store initial and final weights, so that at a glance you
  ||| can see the how much you have added or removed from each
  ||| container.
  |||
  ||| Explanation of User Interaction
  |||
  ||| - We start off in default mode, with no containers and no selection.
  ||| - The user adds a container by entering its barcode
  ||| - The barcode scanner works like a keyboard, so we can't
  |||   distinguish it from key presses.
  |||   - It's configured to prefix the barcode with a `*` symbol.
  |||   - `*` always transfers focus to the character list.
  |||     - subsequent digits are appended to the list
  |||     - enter tries to parse the digits as a barcode, if it succeeds it is focused
  |||       - if it's not in the list, we add a new container to the list
  ||| - if we have a selected container
  |||   - enter will store the current weight as the gross weight of the container
  |||   - delete will store the current weight as the tear weight of the container
  |||   - escape will reset both weights
  record SmartScale where
    constructor MkSmartScale
    containers : VList Raw.Container
    scale      : Result
    barcode    : Maybe Barcode
    image      : Image

  ||| Helper to construct actions which depend on current weight.
  |||
  ||| The common logic is that if the current scale `Result` is not a
  ||| `Weight`, then don't do *anything*. Otherwise, update the
  ||| current container with the appropriate function partially
  ||| applied to the given weight.
  public export
  withCurrentWeight
    :  (Weight -> Raw.Container -> Raw.Container)
    -> SmartScale
    -> IO $ Response _ SmartScale (List Raw.Container)
  withCurrentWeight f self = case self.scale of
    Ok weight => update $ {containers $= update (f weight)} self
    _         => ignore

  ||| Try to select the barcode characters we collected.
  |||
  ||| This is a bit complicated.
  |||
  ||| - If the barcode is invalid, we clear the input field
  ||| - If the barcode is valid user barcode, we try to select it.
  |||   - If it's not present, then we create a new barcode.
  ||| - If the barcode is a valid Food barcode, then we try to select it.
  |||   - If it's not present, no action is taken.
  select : (self : SmartScale) -> (s : Maybe String) -> SmartScale
  select self bc = case join $ validateBarcode <$> bc of
    -- barcode is invalid, but we still need to clear the barcode chars
    Nothing => {barcode := Nothing} self
    -- valid user barcode (which is a container id), select or add.
    Just (User id) => {containers $= findOrInsert' id} self
    -- valid food barcode, just do our best to select it.
    -- XXX: if it's not present, or there's multiple matches, we
    -- should pop up a dialog.
    Just bc => {containers $= find' (hasBarcode bc)} self
  where
    findOrInsert' : Id -> VList Raw.Container -> VList Raw.Container
    findOrInsert' id self = findOrInsert (hasBarcode (User id)) (empty id Reusable) self

    validateBarcode : String -> Maybe Barcode
    validateBarcode value = fromDigits value

  ||| Render the state of the SmartScale component.
  |||
  ||| This demonstrates how to manually lay out items on the screen
  ||| using the packing functions.
  View SmartScale where
    size self = hunion (MkArea 23 4) (size self.containers)
    paint state window self = do
      window <- packLeft Normal window self.image
      window <- packLeft state' window VRule
      window <- packTop  Normal window $ show self.scale
      window <- packTop  Normal window barcode
      window <- packTop  state' window HRule
      ignore $  packTop  state  window self.containers
    where
      state' : State
      state' = demoteFocused state

      barcode : String
      barcode = case self.barcode of
        Nothing => "Scan or Type '*' to enter barcode"
        Just bc => show bc

  ||| Create a new SmartScale with the given list of containers.
  export covering
  smartscale
    :  List Raw.Container
    -> Component (HSum [List Bits8, String, Key]) (List Raw.Container)
  smartscale containers = component (MkSmartScale {
    containers = fromList header containers,
    scale      = Empty,
    barcode    = Nothing,
    -- xxx: qr code for URL to server.
    image      = placeholder "No Image" (MkArea 20 40)
  }) (union [onScale, onImage, onKey]) unavailable where
    0 Events : List Type
    Events = [List Bits8, String, Key]

    header : String
    header = "Barcode      Tear      Gross     Net "

    ||| Update the current scale value when we receive a new packet.
    export
    onScale : Single.Handler SmartScale (List Raw.Container) (List Bits8)
    onScale result self = update $ {scale := decode result} self

    ||| Render the given image path in sixel format when we receive an image event.
    |||
    ||| One issue here is that we have don't have access to the window
    ||| here, so we have to choose a fixed image size to render to. But
    ||| apparently it's important not call out to a subprocess while
    ||| rendering.
    export covering
    onImage : Single.Handler SmartScale (List Raw.Container) String
    onImage path self = continue $ do
      sixel <- sixelFromPath Max path path (MkArea 20 40)
      pure $ {image := sixel} self

    ||| All the fun stuff is in here.
    onKey : Single.Handler {events = Events} SmartScale (List Raw.Container) Key
    onKey (Alpha '*') self = push (textInput "") (select self)
    onKey (Alpha 'q') self = yield $ toList self.containers
    onKey (Alpha 'r') self = update $ {containers $= update $ reset} self
    onKey (Alpha 's') self = withCurrentWeight setGross self
    onKey (Alpha 't') self = withCurrentWeight setTear  self
    onKey Up          self = update $ {containers $= goLeft} self
    onKey Down        self = update $ {containers $= goRight} self
    onKey Delete      self = update $ {containers $= delete} self
    onKey Enter       self = withCurrentWeight setGross self
    onKey Tab         self = update $ {containers $= goRight} self
    onKey Escape      self = exit
    onKey _           self = ignore

  ||| Main entry point1
  export covering
  run : IO ()
  run = Prelude.ignore $ runComponent @{centered} !mainLoop (smartscale [])
  where
    mainLoop : IO (InputShim [List Bits8, String, Key])
    mainLoop = do
      mainLoop <- inputShim
      image <- raw {eventT = String}     "Image"
      scale <- raw {eventT = List Bits8} "Scale"
      pure $ (mainLoop.addEvent image).addEvent scale

||| Entry point for basic scale command.
export partial
main : List String -> IO Builtin.Unit
main ("--once" :: path :: _) = do
  weight <- getWeight path
  putStrLn $ show weight
main _ = run
