||| Types and functions for supported barcode formats.
module Barcode


import Derive.Prelude
import JSON.Derive
import DirDB
import TUI.View


%language ElabReflection
%default total


||| Concrete type for all supported barcode formats.
public export
data Barcode
  = EAN13 (Vect 13 Char)
  | EAN8  (Vect 8  Char)
  | UPC_A (Vect 12 Char)
  | UPC_E (Vect 7  Char)
  | User  (Vect 4  Char)
%runElab derive "Barcode" [Eq,Ord, FromJSON, ToJSON]

||| Convert barcode to a string without length prefix.
export
toString : Barcode -> String
toString (EAN13 bc) = pack $ toList bc
toString (EAN8  bc) = pack $ toList bc
toString (UPC_A bc) = pack $ toList bc
toString (UPC_E bc) = pack $ toList bc
toString (User  bc) = pack $ toList bc

export
Show Barcode where
  show (EAN13 bc) = "EAN13:" ++ pack (toList bc)
  show (EAN8  bc) = "EAN8:"  ++ pack (toList bc)
  show (UPC_A bc) = "UPC-A:" ++ pack (toList bc)
  show (UPC_E bc) = "UPC-E:" ++ pack (toList bc)
  show (User  bc) = "User:"  ++ pack (toList bc)

||| Barcodes are serialized as a string with the standard prefixed.
export
ToJSON Barcode where
  toJSON bc = string $ show bc

-- thanks to Stephen Hoek again, for explaining how to write this. I
-- struggled to figure it out on my own.
--
-- XXX: file doc issue and PR with the upstream library, using this as
-- an example.

||| Helper function for decoding a vector from json. This could
||| probably be moved to Util.idr
parseVect : {k : _} -> Parser String (Vect k Char)
parseVect s = case toVect k (unpack s) of
  Just v  => Right v
  Nothing => fail "Expected String of length \{show k}"

||| Helper function for parsing a the barcode
|||
||| Splits the prefix, and tries to populate the vector of the correct
||| length.
parseBarcode : Parser String Barcode
parseBarcode s = case forget $ split (':' ==) s of
  ["EAN13", r] => EAN13 <$> parseVect r
  ["EAN8",  r] => EAN8  <$> parseVect r
  ["UPC-A", r] => UPC_A <$> parseVect r
  ["UPC-E", r] => UPC_E <$> parseVect r
  ["User", r]  => User  <$> parseVect r
  _            => fail "Invalid barcode: \{s}"

||| Implement JSON Deserialization
export
FromJSON Barcode where
  fromJSON = withString "Barcode" parseBarcode

||| Construct a barcode from an unprefixed string
export
fromDigits : String -> Maybe Barcode
fromDigits s = case length s of
  4  => map User  $ toVect  4 $ unpack s
  7  => map UPC_E $ toVect  7 $ unpack s
  8  => map EAN8  $ toVect  8 $ unpack s
  12 => map UPC_A $ toVect 12 $ unpack s
  13 => map EAN13 $ toVect 13 $ unpack s
  _  => Nothing

||| Allow static strings to decode automatically to barcodes.
|||
||| This is useful for testing and in the REPL, but it will crash if
||| parsing fails.
public export partial
fromString : String -> Barcode
fromString s = case fromDigits s of
  Nothing => idris_crash "Not a valid barcode"
  Just b  => b

||| Implement PathSafe for Barcode
export
PathSafe (Barcode) where
  toPath self@(EAN8  xs) = toMaybe (all isDigit xs) $ show self
  toPath self@(EAN13 xs) = toMaybe (all isDigit xs) $ show self
  toPath self@(UPC_E xs) = toMaybe (all isDigit xs) $ show self
  toPath self@(UPC_A xs) = toMaybe (all isDigit xs) $ show self
  toPath self@(User  xs) = toMaybe (all isDigit xs) $ show self
  fromPath = fromDigits

||| The default view for Barcode is provided by the `show` blanket impl.
export
%hint
viewImpl : View Barcode
viewImpl = show
