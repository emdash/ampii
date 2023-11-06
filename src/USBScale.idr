||| Driver for talking to 510 Scale over USB.
|||
||| The scale sends a 6-byte packet, which must be manually swizzled
||| to decode the weight.
module USBScale


import Data.Bits
import Data.Buffer
import Data.Either
import Data.Vect
import System.Concurrency
import System.File

import Derive.Prelude

import Measures


%language ElabReflection
%default total


||| Move to Utils lib
debug : Show a => a -> IO Builtin.Unit
debug value = do
  _ <- fPutStrLn stderr (show value)
  pure ()

||| Unit to use when we can't determine from the USB traffic
defaultUnit : Unit Mass
defaultUnit = Grams

||| Convert integer unit into type
units : Int -> Unit Mass
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
%runElab derive "Result" [Show,Eq]


||| Calculate the current weight from the raw binary values
calcWeight : Int -> Int -> Int -> Int -> Weight
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
decode : List Int -> Result
decode [report, status, unit, exp, lsb, msb] =
  if report == 0x03
    then case status of
      0x01 => Fault "Fault"
      0x02 => Empty
      0x03 => Weighing
      0x04 => Ok $ calcWeight unit msb lsb exp
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
  withFile path Read onError (loopN buf 5 Nothing)
  where
    loopN : Buffer -> Nat -> Maybe Weight -> File -> IO (Either String Weight)
    loopN buf Z     r file = pure $ maybeToEither "impossible" r
    loopN buf (S n) r file = do
      putStrLn "\{show r}: (\{show n} tries remaining...)"
      _ <- readBufferData file buf 0 6
      d <- bufferData buf
      case decode d of
        Ok w => loopN buf n     (Just w) file
        _    => loopN buf (S n) r        file

    onError : FileError -> IO String
    onError err = pure $ show err


||| Continuously read from the scale into a channel.
partial
run : (Result -> IO Builtin.Unit) -> String -> IO Builtin.Unit
run post path = do
  Just buf <- newBuffer 6
    | Nothing => debug "error, could not allocate buffer"
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
      d <- bufferData buf
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


||| Entry point for basic scale command.
export partial
main : List String -> IO Builtin.Unit
main ("--once" :: path :: _) = do
  weight <- getWeight path
  putStrLn $ show weight
main (path :: _) = do
  chan <- makeChannel
  _ <- spawn path (channelPut chan)
  read chan
  where
    read : Channel Result -> IO Builtin.Unit
    read chan = do
      msg <- channelGet chan
      putStrLn (show msg)
      read chan
main _ = do
  debug "No device file given"
  pure ()
