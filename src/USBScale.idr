||| Driver for talking to 510 Scale over USB.
|||
||| The scale sends a 6-byte packet, which must be manually swizzled
||| to decode the weight.
module USBScale

-- stdlib imports
import Data.Bits
import Data.Buffer
import Data.Either
import Data.Vect
import Derive.Prelude
import System
import System.File.Error
import System.File.ReadWrite
import System.File.Virtual

-- third-party imports
import IO.Async
import IO.Async.Core
import IO.Async.File
import IO.Async.Loop.Epoll
import JSON.Simple.Derive
import TUI
import TUI.Component
import TUI.MainLoop
import TUI.MainLoop.Async

-- application modules
import Barcode
import Container
import Control.ANSI
import Food
import Inventory
import Measures
import Util

%language ElabReflection
%default total


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

||| Reading of scale IO operation
public export
data Reading
  = Empty
  | Weighing
  | Fault String
  | Ok Weight
%runElab derive "Reading" [Show,Ord,Eq,FromJSON,ToJSON]

||| Subtract a tear weight from the reading.
tear : Reading -> Weight -> Reading
tear (Ok w) t = Ok $ w - t
tear r      _ = r

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
decode : List Bits8 -> Reading
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

||| An EventSource representing a USB scale.
|||
||| Reads from the scale device, sending scale readings to the event
||| queue.
partial
scale : Has Reading evts => String -> EventSource evts
scale path queue = try [onErrno] $ do
  withFile path flags 0 loop
where
  flags : Flags
  flags = O_RDONLY <+> O_NONBLOCK

  loop : Fd -> Throws [Errno] ()
  loop fd = do
    bytes <- readnb fd ByteString 6
    stdoutLn $ show bytes
    weakenErrors $ putEvent queue $ decode $ unpack bytes
    loop fd

  onErrno : Catch () Errno
  onErrno err = do
    putEvent queue $ Fault $ show err
    stderrLn "Scale thread exiting"

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
    ||| Inventory database handle
    inventory  : Inventory
    ||| Current scale reading
    scale      : Reading
    ||| Last Barcode Entered By User
    barcode    : Maybe Barcode
    ||| Current Image
    image      : Image

  export
  (.selected) : SmartScale -> Maybe (Either Food Joined.Container)
  (.selected) self = join $ self.inventory.lookupBarcode <$> self.barcode

  (.updateContainer)
    :  SmartScale
    -> Container.Id
    -> (Raw.Container -> Raw.Container)
    -> SmartScale
  (.updateContainer) self id f = {
    inventory := self.inventory.updateContainer id f
  } self

  export
  (.updateFood)
    :  SmartScale
    -> Barcode
    -> (Food -> Food)
    -> SmartScale
  (.updateFood) self barcode f = {
    inventory := self.inventory.updateFood barcode f
  } self

  ||| Update the selected item if it is a container
  export
  (.updateSelectedContainer)
   :  SmartScale
   -> (Raw.Container -> Raw.Container)
   -> IO $ Response _ SmartScale Inventory
  (.updateSelectedContainer) self f = case self.barcode of
    Just (User id) => update $ { inventory $= \x => x.updateContainer id f } self
    _              => ignore -- XXX: this is I would ideally warn the user unobtrusively if it
                             --  was convenient.

  ||| Update the selected item if it is a food
  (.updateSelectedFood)
   :  SmartScale
   -> (Food -> Food)
   -> IO $ Response _ SmartScale Inventory
  (.updateSelectedFood) self f = case self.barcode of
    Nothing       => ignore
    Just (User _) => ignore
    Just barcode  => update $ { inventory $= \x => x.updateFood barcode f } self

  ||| Helper to construct actions which depend on current weight.
  |||
  ||| The common logic is that if the current scale `Result` is not a
  ||| `Weight`, then do nothing.
  |||
  ||| Otherwise, update the current container with the function partially
  ||| applied to the given weight.
  export
  (.withWeight)
    :  SmartScale
    -> (Weight -> Raw.Container -> Raw.Container)
    -> (Raw.Container -> Raw.Container)
  (.withWeight) self f = case self.scale of
      Ok weight => (f weight)
      Empty     => (f 0.g)
      _         => id

  ||| Helper to construct actions which depend on current weight.
  |||
  ||| The common logic is that if the current scale `Result` is not a
  ||| `Weight`, then do nothing.
  |||
  ||| Otherwise, update the current container with the function partially
  ||| applied to the given weight.
  export
  (.updateSelectedWithWeight)
    :  SmartScale
    -> (Weight -> Raw.Container -> Raw.Container)
    -> IO $ Response _ SmartScale Inventory
  (.updateSelectedWithWeight) self f = self.updateSelectedContainer $ self.withWeight f

{-
  export
  (.tearFromNet)
    :  SmartScale
    -> Maybe Double
    -> SmartScale
  (.tearFromNet) self Nothing    = self
  (.tearFromNet) self (Just net) = self.updateSelectedPure $ setTearFromNet $ net.g
-}

  ||| Render the state of the SmartScale component.
  |||
  ||| This demonstrates how to manually lay out items on the screen
  ||| using the packing functions.
  View SmartScale where
    size self = hunion (MkArea 23 4) (MkArea 1 self.inventory.size)
    paint state window self = do
      let barcodes = self.inventory.barcodes
      let bcsize = sizeVertical barcodes
      let (left, window) = window.splitLeft bcsize.width
      ignore $  paintVertical Normal left barcodes
      window <- packLeft state' window VRule
      window <- packTop  Normal window $ show curWeight
      window <- packTop  Normal window barcode
      window <- packTop  state' window HRule
      selection window
    where
      state' : State
      state' = demoteFocused state

      ||| XXX: add to TUI.Layout
      centerIn : Rect -> String -> Context ()
      centerIn window s = paint state' ((size s).centerIn window) s

      selection : Rect -> Context ()
      selection window = case self.barcode of
        Nothing => centerIn window "No Barcode"
        Just s  => case self.selected of
          Nothing => centerIn window "Unknown Container Id \{show s}"
          selected => paint state' window selected

      curWeight : Reading
      curWeight = case self.selected of
        Nothing => self.scale
        Just (Left food) => self.scale
        Just (Right cont) => tear self.scale cont.tear

      barcode : String
      barcode = case self.barcode of
        Nothing => "Scan or Type '*' to enter barcode"
        Just bc => show bc

  ||| Create a new SmartScale with the given list of containers.
  export covering
  smartscale
    :  Inventory
    -> Component (HSum [Reading, Key]) Inventory
  smartscale inventory = component (MkSmartScale {
    inventory  = inventory,
    scale      = Empty,
    barcode    = Nothing,
    -- xxx: qr code for URL to server.
    image      = placeholder "No Image" (MkArea 20 40)
  }) (union [onScale, {-onImage,-} onKey]) unavailable where
    0 Events : List Type
    Events = [Reading, {-String,-} Key]

    header : String
    header = "Barcode      Tear      Gross     Net "

    ||| Update the current scale value when we receive a new packet.
    export
    onScale : Single.Handler SmartScale Inventory Reading
    onScale result self = update $ {scale := result} self

    ||| Render the given image path in sixel format when we receive an image event.
    |||
    ||| One issue here is that we have don't have access to the window
    ||| here, so we have to choose a fixed image size to render to. But
    ||| apparently it's important not call out to a subprocess while
    ||| rendering.
    export covering
    onImage : Single.Handler SmartScale Inventory String
    onImage path self = continue $ do
      sixel <- sixelFromPath Max path path (MkArea 20 40)
      pure $ {image := sixel} self

    ||| All the fun stuff is in here.
    onKey : Single.Handler {events = Events} SmartScale Inventory Key
    onKey (Alpha '*') self = push (textInput "") (\x => {barcode := fromDigits =<< x} self)
    onKey (Alpha 'q') self = yield $ self.inventory
    onKey (Alpha 'r') self = self.updateSelectedContainer  reset
    onKey (Alpha 't') self = self.updateSelectedWithWeight setTear
    onKey (Alpha 's') self = continue $ do
      save "./inventory.json" self.inventory
      pure $ self
    -- onKey (Alpha 'T') self = push (numeric 0.0) self.tearFromNet
    -- onKey Up          self = update $ {containers $= goLeft} self
    -- onKey Down        self = update $ {containers $= goRight} self
    -- onKey Delete      self = update $ {containers $= delete} self
    onKey Enter       self = self.updateSelectedWithWeight setGross
    -- onKey Tab         self = update $ {containers $= goRight} self
    onKey Escape      self = exit
    onKey _           self = ignore

  ||| Main entry point1
  export covering
  run : Maybe String -> IO Builtin.Unit
  run path = do
    result <- runComponent @{centered} mainLoop (smartscale $ fromMaybe empty !(load "./inventory.json"))
    case result of
      Nothing => pure ()
      Just inventory => save "inventory.json" inventory
  where
    dummy : Has Reading evts => Reading -> EventSource evts
    dummy value queue = putEvent queue value

    mainLoop : AsyncMain [Reading, Key]
    mainLoop = case path of
      Nothing   => asyncMain [dummy (Fault "no device")]
      Just path => asyncMain [scale path]

||| Entry point for basic scale command.
export partial
main : List String -> IO Builtin.Unit
main []     = run Nothing
main [path] = run $ Just path
main ["test", bc] = putStrLn $ show $ the (Maybe Barcode) (fromDigits bc)
main _ = putStrLn "expected 0 or 1 argument"
