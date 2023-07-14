
||| Driver for talking to 510 Scale over USB.
|||
||| The scale sends a 6-byte packet, which must be manually swizzled
||| to decode the weight.
module USBScale


import Data.Bits
import Data.Buffer
import Data.Vect
import System.Concurrency
import System.File

import Derive.Prelude


%language ElabReflection
%default total


units : Int -> String
units 0  = "unknown"
units 1  = "mg"
units 2  = "g"
units 3  = "kg"
units 4  = "cd"
units 5  = "taels"
units 6  = "gr"
units 7  = "dwt"
units 8  = "tonnes"
units 9  = "tons"
units 10 = "ozt"
units 11 = "oz"
units 12 = "lbs"
units _  = "unknown"


data Status
  = Empty
  | Weighing
  | Ok String Double
%runElab derive "Status" [Show,Eq]

ScaleResult : Type
ScaleResult = Either String Status


public export
data Event
  = Ready
  | Weight String Double
%runElab derive "Event" [Show,Eq]


calcWeight : Int -> Int -> Int -> Double
calcWeight msb lsb exponent =
  let
    mantissa = (cast ((msb `shiftL` 8) .|. lsb)) / 10.0
  in
    case exponent of
      0x00 => cast mantissa
      0xff => cast mantissa
      _    => pow (cast mantissa) (cast exponent)


data State
  = WaitForWeight
  | WaitForEmpty


transition : State -> Status -> (State, Maybe Event)
transition WaitForWeight (Ok u w) = (WaitForEmpty,  Just $ Weight u w)
transition WaitForWeight _        = (WaitForWeight, Nothing)
transition WaitForEmpty  Empty    = (WaitForWeight, Just Ready)
transition WaitForEmpty  _        = (WaitForEmpty,  Nothing)


decode : List Int -> ScaleResult
decode [report, status, unit, exp, lsb, msb] =
  if report == 0x03
    then case status of
      0x01 => Left    "Fault"
      0x02 => Right   Empty
      0x03 => Right   Weighing
      0x04 => Right $ Ok (units unit) (calcWeight msb lsb exp)
      0x05 => Left    "Negative Weight"
      0x06 => Left    "Overweight"
      0x07 => Left    "Recalibrate"
      0x08 => Left    "Rezero"
      _    => Left  $ "Unknown status code: " ++ show status
    else Left "Error Reading Scale!"
decode _ = Left "Error, invalid packet"


export partial
run : State -> Buffer -> File -> IO (Either String ())
run state buf file = do
  bytes <- readBufferData file buf 0 6
  case bytes of
    Right 6 => do
      d <- bufferData buf
      case decode d of
        Left err => pure $ Left err
        Right res => do
          let (state, event) = transition state res
          case event of
            Just event => putStrLn $ show event
            Nothing => pure ()
          run state buf file
    other => pure $ Left "Error reading from buffer"


export partial
main : List String -> IO ()
main (path :: rest) = do
  Just buf <- newBuffer 6
           | Nothing => putStrLn "error, could not allocate buffer"
  Right _ <- withFile path Read onError (run WaitForEmpty buf)
          | Left err => putStrLn err
  pure ()
  where
    onError : FileError -> IO String
    onError err = pure $ show err
main _ = do
  putStrLn "No device file given"
