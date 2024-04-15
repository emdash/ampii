||| Minimalist terminal UI framework.
|||
||| A Numeric Input Widget
module TUI.View.Numeric


import Data.Fin
import Data.String
import Data.SnocList
import TUI.View
import Util


%default total


||| The subset of possible keys that we handle.
data Input
  = Digit (Fin 10)
  | Dot
  | Minus

charToDigit : Char -> Maybe (Fin 10)
charToDigit char = integerToFin (cast $ ord char - ord '0') 10

digitToChar : Fin 10 -> Char
digitToChar d = cast $ (ord '0') + cast (finToNat d)

||| An editable string of digits, with or without a decimal point.
data Digits
  = Integral (SnocList (Fin 10))
  | Decimal  (SnocList (Fin 10)) (SnocList (Fin 10))

||| Insert a single digit value.
insertDigits : Fin 10 -> Digits -> Digits
insertDigits d (Integral ds)  = Integral (ds :< d)
insertDigits d (Decimal i ds) = Decimal  i (ds :< d)

||| An empty digits value.
export empty : Digits ; empty = Integral [<]

||| Convert a digit string to a string
digitsToString : SnocList (Fin 10) -> String
digitsToString [<] = "0"
digitsToString xs = kcap $ digitToChar <$> xs

||| An editable number widget.
|||
||| This filters out non-numeric keypresses, and cannot hold a
||| non-numeric value.
|||
||| Other features include:
||| - press `-` at any time to swap sign
||| - dot ignored after first press.
||| - backspace clears whole input (as it's usually easier to start again).
export
record Numeric a where
  constructor N
  digits : Digits
  sign   : Bool
  step   : a

||| Get the width of the entire control, including symbols and padding.
width : Digits -> Nat
width (Integral  ds)    = 3 + length ds
width (Decimal   is ds) = max 6 (3 + length is + 1 + length ds)

||| Insert a single numeric key
insert : Input -> Numeric a -> Numeric a
insert (Digit d) self = { digits $= insertDigits d } self
insert Dot       self = case self.digits of
  Integral ds   => { digits := Decimal ds [<] } self
  Decimal  _  _ => self
insert Minus     self = { sign $= not } self

||| Get the string represntation of the numeric widget.
export
toString : Numeric a -> String
toString (N (Integral xs) sign _) =
  if sign
    then "-\{digitsToString xs}"
    else " \{digitsToString xs}"
toString (N (Decimal integer decimal) sign _) =
  if sign
    then "-\{digitsToString integer}.\{digitsToString decimal}"
    else " \{digitsToString integer}.\{digitsToString decimal}"

toNat : Num a => Numeric a -> Maybe a
toNat = parsePositive . toString

toInteger : Num a => Neg a => Numeric a -> Maybe a
toInteger = parseInteger . toString

toDouble : Numeric a -> Maybe Double
toDouble = parseDouble . toString

||| Generic paint function for all variants of numeric widget.
paintNumeric : Char -> State -> Rect -> Numeric a -> IO ()
paintNumeric symbol state window self = do
  showCharAt window.nw symbol
  case state of
      Focused => reverseVideo
      _       => pure ()
  showTextAt (window.nw + MkArea 2 0) (toString self)
  sgr [Reset]

||| Helper function for handleCommon
handleChar : (Char -> Maybe Input) -> Char -> Numeric a -> Numeric a
handleChar f char self = case f char of
  Just i  => insert i self
  Nothing => self

||| Factor out event handling logic common to all variants.
handleCommon
  : Key
  -> (Char -> Maybe Input)
  -> Numeric a
  -> Response (Numeric a) action
handleCommon (Alpha char) f = Update . (handleChar f char)
handleCommon Delete       _ = Update . { digits := empty }
handleCommon Left         _ = const FocusParent
handleCommon Enter        _ = const FocusParent
handleCommon Escape       _ = const FocusParent
handleCommon Tab          _ = const FocusNext
handleCommon _            _ = Update . id

-- the unicode symbols which decorate the widget
natSymbol  : Char ; natSymbol  = cast 0x2115
intSymbol  : Char ; intSymbol  = cast 0x2124
realSymbol : Char ; realSymbol = cast 0x211d

||| This implementation ignores decimals and minus signs.
export
View (Numeric Nat) where
  size self = MkArea (width self.digits) 1
  paint state window self = paintNumeric natSymbol state window self
  handle key = handleCommon key $ (map Digit) . charToDigit

||| This implementation ignores decimals, but handles the minus sign.
export
View (Numeric Integer) where
  size self = MkArea (width self.digits) 1
  paint state window self = paintNumeric intSymbol state window self
  handle key = handleCommon key special
    where
      special : Char -> Maybe Input
      special '-' = Just Minus
      special c   = Digit <$> charToDigit c

||| This implementation handles both decimal and minus sign.
export
View (Numeric Double) where
  size self = MkArea (width self.digits) 1
  paint state window self = paintNumeric realSymbol state window self
  handle key = handleCommon key special
    where
      special : Char -> Maybe Input
      special '-' = Just Minus
      special '.' = Just Dot
      special c   = Digit <$> charToDigit c

||| Create a numeric widget from a number value.
export
numeric : a -> a -> Numeric a
numeric value step = N {
  digits = empty, -- xxx: decode
  sign   = False,
  step   = step
}
