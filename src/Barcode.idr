||| Types and functions for supported barcode formats.
module Barcode


import Derive.Prelude
import JSON.Derive


%language ElabReflection
%default total


||| Concrete type for all supported barcode formats.
export
data Barcode
  = EAN13 (Vect 13 Char)
  | UPC   (Vect 11 Char)
%runElab derive "Barcode" [Eq,Ord]

export
Show Barcode where
  show (EAN13 bc) = "EAN13:" ++ pack (toList bc)
  show (UPC   bc) = "UPC:" ++ pack (toList bc)

||| Barcodes are serialized as a string with the standard prefixed.
|||
||| This should correspond to whatever zbar uses.
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
  ["EAN13",r] => EAN13 <$> parseVect r
  ["UPC",r]   => UPC   <$> parseVect r
  _           => fail "Invalid barcode: \{s}"

||| Implement JSON Deserialization
export
FromJSON Barcode where
  fromJSON = withString "Barcode" parseBarcode

||| Construct a barcode from an unprefixed string
export
fromDigits : String -> Maybe Barcode
fromDigits s = case length s of
  11 => map UPC   $ toVect 11 $ unpack s
  13 => map EAN13 $ toVect 13 $ unpack s
  _  => Nothing
