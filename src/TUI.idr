||| Minimalist terminal UI framework.
|||
||| I can't get ncurses-idris working, so I'm rolling this pure-idris
||| alternative.
|||
||| It's higher-level than ncurses, with the goal of being able to
||| quickly create keyboard-driven interfaces that are 'good enough'
||| for experimentation and prototyping, with an eye toward efficient
||| data entry.
|||
||| The lack of ncurses support does pose some challenges. In
||| particular, it's not possible to distinguish between the start of
||| an escape sequence or the user pressing the escape key, as there
||| is no non-blocking way to get the next character from stdin.
|||
||| Other limitations include:
||| - no support for termcap or terminfo,
|||   - no checking or fallback.
|||
||| The primary advantage is sheer simplicity: no dependencies are
||| required beyond `contrib`, so long as you rely on an
||| ANSI-compatible terminal or emulator.
module TUI

import public TUI.Painting
import public TUI.View
import public TUI.View.Menu
import public TUI.View.TextInput

import Data.Vect
import Data.Vect.Quantifiers
import System
import System.File
import System.Signal
import Derive.Prelude
import Util
import Zipper


%default total
%language ElabReflection


||| Functions to consider moving elsewhere
namespace Util

  ||| Truncate a string to the given length.
  public export
  truncateTo : Nat -> String -> String
  truncateTo n s = pack $ take n $ unpack s

  ||| Format a string into an exact width
  |||
  ||| Pad if length is shorter than width, truncate if longer.
  public export
  frameString : Nat -> String -> String
  frameString w s =
    let l = length s
    in case (l < length s) of
      True  => padRight w ' ' s
      False => truncateTo w s




namespace Numeric
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
    -> Response (Numeric a)
  handleCommon (Alpha char) f = Update . (handleChar f char)
  handleCommon Delete       _ = Update . { digits := empty }
  handleCommon Left         _ = const FocusParent
  handleCommon Enter        _ = const FocusParent
  handleCommon Escape       _ = const FocusParent
  handleCommon Tab          _ = const FocusNext
  handleCommon _            _ = Update . id

  ||| This implementation ignores decimals and minus signs.
  export
  View (Numeric Nat) where
    size self = MkArea (width self.digits) 1
    paint state window self = paintNumeric (cast 0x2115) state window self
    handle key = handleCommon key $ (map Digit) . charToDigit

  ||| This implementation ignores decimals, but handles the minus sign.
  export
  View (Numeric Integer) where
    size self = MkArea (width self.digits) 1
    paint state window self = paintNumeric (cast 0x2124) state window self
    handle key = handleCommon key special
      where
        special : Char -> Maybe Input
        special '-' = Just Minus
        special c   = Digit <$> charToDigit c

  ||| This implementation handles both decimal and minus sign.
  export
  View (Numeric Double) where
    size self = MkArea (width self.digits) 1
    paint state window self = paintNumeric (cast 0x211D) state window self
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


namespace Table
  data Mode
    = Default
    | EditCell
    | AddRow

  public export
  record Cell ty where
    view : ty
    {auto impl : View ty}

  record Table (tys : Vect k Type) where
    headers : Vect k (String, Nat)
    rows    : Zipper (All Cell tys)
    row     : Nat
    col     : Fin k
    mode    : Table.Mode

-- missing widgets:
-- table
-- SOP widget


||| Associated definitions for `Form`.
namespace Form
  ||| A single field in a form.
  |||
  ||| It is a labeled value that wraps an inner view, capturing its
  ||| View implementation.
  public export
  record Field ty where
    constructor F
    label : String
    view : ty
    {auto impl : View ty}

  ||| Get the area of the field's wrapped view, not including its
  ||| label.
  viewSize : Field ty -> Area
  viewSize self = size @{self.impl} self.view

  ||| Update field's wrapped view in response to a key event.
  handleView : Key -> Field ty -> Response (Field ty)
  handleView k f = case handle @{f.impl} k (f.view) of
    Update new  => Update $ { view := new } f
    FocusParent => FocusParent
    FocusNext   => FocusNext

  ||| A form displays a set of views, each with a string label.
  |||
  ||| One field has focus, and user input is routed to this sub-view.
  export
  record Form (tys : Vect k Type) where
    constructor MkForm
    fields : All Field tys
    focused : Fin k
    split : Nat
    editing : Bool
    contentSize : Area

  parameters {k : Nat} {tys : Vect k Type}
    ||| Get the character width of the longest label in the form.
    maxLabelWidth : All Field tys -> Nat
    maxLabelWidth tys = reduceAll max (length . (.label)) 0 tys

    ||| Calculate the size the form widgets (not including the labels)
    export
    sizeViewsVertical : All Field tys -> Area
    sizeViewsVertical fields = reduceAll hunion (viewSize) (MkArea 0 0) fields

  ||| Render the form's fields vertically.
  export
  paintVertical
    : {k : Nat}
    -> {tys : Vect k Type}
    -> State
    -> Rect
    -> Form tys -> IO ()
  paintVertical state window self = do
    loop 0 window self.fields
    where
      loop
        : {k : Nat}
        -> {tys : Vect k Type}
        -> Nat
        -> Rect
        -> All Field tys -> IO ()
      loop _  _ [] = pure ()
      loop i  window (x :: xs) = do
        let (top, bottom) = vsplit window (viewSize x).height
        let (left, right) = hsplit top    (self.split + 3)
        let left  = inset left  (MkArea 1 0)
        let right = inset right (MkArea 1 0)
        case (i == (finToNat self.focused), state) of
          (True, Focused) => do
            if self.editing
              then sgr [SetStyle SingleUnderline]
              else reverseVideo
            showTextAt left.nw x.label
            sgr [Reset]
            if self.editing
              then paint @{x.impl} Focused right x.view
              else paint @{x.impl} Normal  right x.view
          _ => do
            showTextAt left.nw x.label
            paint @{x.impl} Normal right x.view
        loop (S i) bottom xs

  ||| Dispatch keyboard input to the currently-focused subview.
  |||
  ||| This has to handle `Escape`ing if the subview escapes.
  export
  handleNth
    : {k : Nat}
    -> {tys : Vect k Type}
    -> Fin k
    -> Key
    -> All Field tys
    -> Response (All Field tys)
  handleNth FZ key (f :: fs) = case handleView key f of
    Update new  => Update $ new :: fs
    FocusParent => FocusParent
    FocusNext   => FocusNext
  handleNth (FS i) key (f :: fs) = case handleNth i key fs of
    Update fs => Update $ f :: fs
    FocusParent => FocusParent
    FocusNext  => FocusNext

  parameters {k : Nat} {tys : Vect k Type}
    ||| Move the form to the next focused value.
    export
    nextChoice : Form tys -> Form tys
    nextChoice = { focused $= finS }

    ||| Move the form to the previous focused value.
    export
    prevChoice : Form tys -> Form tys
    prevChoice = { focused $= predS }

    ||| Dispatch event to the selected field.
    |||
    ||| We may need to update our editing state in response.
    export
    handleEditing : Key -> Form tys -> Response (Form tys)
    handleEditing key self = case handleNth self.focused key self.fields of
      Update fields => Update $ { fields  := fields } self
      FocusParent   => Update $ { editing := False }  self
      FocusNext     => Update $ nextChoice            self

    ||| Handles events when in navigation mode.
    |||
    ||| Up/Down change the form focus, various other keys toggle the
    ||| editing state.
    export
    handleDefault : Key -> Form tys -> Response (Form tys)
    -- handleDefault Up _ impossible
    handleDefault Up     = Update . prevChoice
    handleDefault Down   = Update . nextChoice
    handleDefault Tab    = Update . nextChoice
    handleDefault Right  = Update . { editing := True }
    handleDefault Enter  = Update . { editing := True }
    handleDefault Escape = const FocusParent
    handleDefault Left   = const FocusParent
    handleDefault _      = Update . id

  ||| The View implementation for form renders each labeled sub-view
  ||| vertically.
  |||
  ||| The labels are left-justified, while the sub-views are
  ||| left-justified, and aligned relative to the widest label in the
  ||| form.
  |||
  ||| Only one sub-view has focus. Tab is used to move focus to the
  ||| next form field.
  export
  {k : Nat} -> {tys : Vect (S k) Type} -> View (Form tys) where
    size self = self.contentSize + MkArea self.split 1

    paint state window self = do
      let height = window.size.height `minus` 2
      vline (window.nw + MkArea 0 1)                height
      vline (window.nw + MkArea (self.split + 3) 1) height
      case state of
        Focused => box window
        _       => pure ()
      paintVertical state (shrink window) self

    -- dispatch events depending on editing state
    handle key self = case self.editing of
      True => handleEditing key self
      False => handleDefault key self


  ||| Construct a form from a list of field records
  public export
  form
    : {k : Nat}
    -> {tys : Vect (S k) Type}
    -> All Field tys
    -> Form tys
  form fields = MkForm {
    fields      = fields,
    focused     = 0,
    split       = (maxLabelWidth fields),
    contentSize = (sizeViewsVertical fields),
    editing     = False
  }


||| Low-level TUI application mainloop.
|||
||| Manages terminal state, handles OS-level signals, and receives
||| keyboard events from STDIN.
|||
||| Use this entry point if you do not want escape-sequence decoding.
covering
runRaw
  :  (Char -> state -> Maybe state)
  -> (state -> IO Builtin.Unit)
  -> state
  -> IO state
runRaw handler render init = do
  -- default SigINT handler doesn't clean up raw mode, so we need to
  -- handle it explicitly and make sure to clean up.
  Right _ <- collectSignal SigINT
           | Left err => die "couldn't trap SigINT"

  -- run mainloop
  hideCursor
  saveCursor
  ret <- withRawMode err (loop init)
  cleanup
  pure ret
where
  -- restore terminal state as best we can
  cleanup : IO ()
  cleanup = do
    restoreCursor
    showCursor

  -- ensure we restore terminal state on IO error
  err : e -> IO state
  err _ = do
    cleanup
    die "an unhandled error occured"

  -- this is the actual recursive mainloop. The unusual `()` in the
  -- signature allows loop to be partially-applied above.
  loop : state -> () -> IO state
  loop s () = do
    -- repaint the screen with the current state
    clearScreen
    moveTo (MkPos 0 0)
    render s

    -- Return immediately if SigINT was received. Nothing, in this
    -- case, means no signal, so continue normal operation.
    --
    -- If we ever need to handle some other signal, like SIGWINCH,
    -- it would be done here.
    Nothing <- handleNextCollectedSignal
             | Just SigINT => pure s
             | Just _      => die "unexpected signal"

    -- handle next key press
    case handler !getChar s of
      Nothing => pure s -- we are done, quit
      Just s  => loop s () -- go to next iteration.


||| Run a raw TUI application, decoding input escape sequences.
|||
||| Use this function if you want escape sequence decoding, but do not
||| want to use the view abstraction for rendering screen contents.
covering export
runTUI
  :  (Key -> state -> Maybe state)
  -> (state -> IO ())
  -> state
  -> IO state
runTUI handler render init = do
  ret <- runRaw (interpretEsc handler) (render . unwrapEsc) (wrapEsc init)
  pure $ unwrapEsc ret


||| Run a TUI application.
|||
||| Use this entry point if you want to use the `View` abstraction.
covering export
runView : View state => state -> IO state
runView init = do
  result <- runTUI wrapView (paint Focused !(screen)) init
  pure result
where
  wrapView : Key -> state -> Maybe state
  wrapView k s = case handle k s of
    Update s    => Just s
    -- effectively ignore these cases, since we're at the root.
    FocusParent => Just s
    FocusNext   => Just s


--- tests

testMenu : Menu String
testMenu = menu ["foo", "bar", "baz"]

testForm : Form [Menu String, Menu String, TextInput, TextInput]
testForm = form [
  F "F1" testMenu,
  F "Long name" testMenu,
  F "Text Input" (fromString "test"),
  F "Test" (fromString "test")
]

partial export
test : IO ()
test = do
  v <- runView $ form [
    F "menu" testMenu,
    F "Nat" $ numeric (the Nat 5) 1,
    F "Integer" $ numeric (the Integer 5) 1,
    F "Double"  $ numeric ( the Double 5.0) 0.1,
    F "nested" testForm
  ]
  putStrLn ""
