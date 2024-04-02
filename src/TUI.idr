||| Minimalist terminal UI framework.
|||
||| I can't get ncurses-idris working, so I'm rolling this pure-idris
||| replacement.
module TUI


import Data.HVect
import System
import System.File
import System.Signal
import Control.ANSI
import Derive.Prelude


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

  ||| Find the unsigned difference between two natural numbers.
  public export
  diff : Nat -> Nat -> Nat
  diff a b = case a < b of
    True  => b `minus` a
    False => a `minus` b

  ||| Decrement the given `Fin` without changing the bound.
  public export
  pred : Fin n -> Fin n
  pred FZ = FZ
  pred (FS k) = weaken k


||| Functions and types having to do with terminal escape sequences.
|||
||| This is best-effort, patches welcome.
namespace EscapeSequences

  ||| Wrapper type for tracking the escape sequences received from
  ||| STDIN.
  export
  data EscState state
    = HaveEsc (SnocList Char) state
    | Default state

  ||| Abstract key value, decoded from ANSI escape sequence.
  public export
  data Key
    = Alpha Char
    | Left
    | Right
    | Up
    | Down
    | Delete
    | Enter
    | Tab
    | Escape
  %runElab derive "Key" [Ord, Eq, Show]

  ||| Track escape sequences for the inner state.
  export
  wrapEsc : state -> EscState state
  wrapEsc = Default

  ||| Project the wrapped value from the escape sequence state.
  export
  unwrapEsc : EscState state -> state
  unwrapEsc (HaveEsc _ s) = s
  unwrapEsc (Default   s) = s

  ||| It's handy to be able to `show` the escape state for debugging.
  export
  Show s => Show (EscState s) where
    show = show . unwrapEsc

  ||| Decode well-known escape sequences into abstract keys.
  |||
  ||| In particular, we handle the cursor keys.
  export
  decodeEsc : SnocList Char -> Maybe (Maybe Key)
  decodeEsc [<]          = Just Nothing
  decodeEsc [< '[']      = Just Nothing
  decodeEsc [< '[', 'C'] = Just $ Just Right
  decodeEsc [< '[', 'D'] = Just $ Just Left
  decodeEsc [< '[', 'A'] = Just $ Just Up
  decodeEsc [< '[', 'B'] = Just $ Just Down
  decodeEsc [< '\ESC']   = Just $ Just Escape
  decodeEsc _            = Nothing


  ||| Interpret console escape sequences as keys.
  |||
  ||| Note: this falls down if the user presses the escape key, because
  ||| we can't tell the difference between the key press and the start
  ||| of an escape sequence. It'd be better to use ncurses.
  export
  interpretEsc
    :  (Key -> state -> Maybe state)
    -> Char
    -> EscState state
    -> Maybe (EscState state)
  interpretEsc f c (HaveEsc esc s) = case (decodeEsc $ esc :< c) of
    Just Nothing    => Just $ HaveEsc (esc :< c) s
    Just (Just Escape) => Nothing
    Just (Just key) => Default <$> (f key s)
    Nothing         => Just $ Default s
  interpretEsc f '\ESC' (Default s) = Just $ HaveEsc [<] s
  interpretEsc f '\DEL' (Default s) = map Default $ f Delete    s
  interpretEsc f '\n'   (Default s) = map Default $ f Enter     s
  interpretEsc f '\t'   (Default s) = map Default $ f Tab       s
  interpretEsc f c      (Default s) = map Default $ f (Alpha c) s


||| Text-centric versions of geometric notions like Pos, Area, and
||| Rect.
namespace Geometry
  ||| The location of a character cell on the screen.
  public export
  record Pos where
    constructor MkPos
    x : Nat
    y : Nat
  %runElab derive "Pos" [Eq, Ord, Show]

  ||| The dimensions of a screen view
  public export
  record Area where
    constructor MkArea
    width : Nat
    height : Nat
  %runElab derive "Area" [Eq, Ord, Show]

  ||| Adding a point to an area returns a new point.
  public export
  (+) : Pos -> Area -> Pos
  (+) (MkPos x y) (MkArea w h) = MkPos (x + w) (y + h)

  ||| The difference between two locations defines an area
  public export
  (-) : Pos -> Pos -> Area
  (-) a b = MkArea (a.x `diff` b.x) (a.y `diff` b.y)

  ||| A width and height without a location.
  namespace Area
    ||| Combine two areas to yield an area that contains both
    export
    union : Area -> Area -> Area
    union a b = MkArea (max a.width a.width) (max a.height b.height)

    ||| Pack areas vertically
    export
    hunion : Area -> Area -> Area
    hunion a b = MkArea (max a.width b.width) (a.height + b.height)

    ||| Pack areas horizontally
    export
    vunion : Area -> Area -> Area
    vunion a b = MkArea (a.width + b.width) (max a.height b.height)

  ||| A rectangular screen region.
  |||
  ||| This is a useful concept for layout. We can abstractly refer to
  ||| the different corners of the box.
  public export
  record Rect where
    constructor MkRect
    pos  : Pos
    size : Area
  %runElab derive "Rect" [Eq, Ord, Show]

  ||| Associated definitiosn for `Rect`.
  namespace Rect
    ||| Northwest corner of the given rect
    export
    (.nw) : Rect -> Pos
    (.nw) b = b.pos

    ||| Northeast corner of the given rect
    export
    (.ne) : Rect -> Pos
    (.ne) b = b.pos + MkArea b.size.width 0

    ||| Northwest corner of the given rect
    export
    (.se) : Rect -> Pos
    (.se) b = b.pos + b.size

    ||| Southwest corner of the given rect
    export
    (.sw) : Rect -> Pos
    (.sw) b = b.pos + MkArea 0 b.size.height

    ||| The column of the left side of the rect
    export
    (.w) : Rect -> Nat
    (.w) b = b.pos.x

    ||| The column of the east side of the rect
    export
    (.e) : Rect -> Nat
    (.e) b = b.pos.x + b.size.width

    ||| The row of the north side of the rect
    export
    (.n) : Rect -> Nat
    (.n) b = b.pos.y

    ||| The row of the south side of the rect.
    export
    (.s) : Rect -> Nat
    (.s) b = b.pos.y + b.size.height

    ||| Return the smallest rectangle which contains the two points.
    export
    fromPoints : Pos -> Pos -> Rect
    fromPoints a b = MkRect (min a b) (a - b)

    ||| Split horizontally at `w` and return the pieces
    export
    hsplit : Rect -> Nat -> (Rect, Rect)
    hsplit b w =
      let
        left  = MkRect b.nw (MkArea w b.size.height)
        right = fromPoints (b.nw + MkArea w 0) b.se
      in
        (left , right)

    ||| Split vertically at `h` and return the pieces
    export
    vsplit : Rect -> Nat -> (Rect, Rect)
    vsplit b h =
      let
        top = MkRect b.nw (MkArea b.size.width h)
        bot = fromPoints (b.nw + MkArea 0 h) b.se
      in
        (top , bot)

    ||| The smallest bounding box fully containing both rectangles.
    export
    union : Rect -> Rect -> Rect
    union a b =
      let
        tl = min a.nw b.nw
        br = min a.se b.se
      in
        fromPoints tl br

    ||| Rectangles form a semigroup with the union operation.
    export
    Semigroup Rect where
      (<+>) = union

  ||| A common default size of terminal window.
  public export
  r80x24 : Rect
  r80x24 = MkRect (MkPos 0 0) (MkArea 80 24)


||| Functions and related to putting text on the screen
namespace Painting
  ||| Move the cursor to the given point
  export
  moveTo : Pos -> IO ()
  moveTo pos = putStr $ cursorMove pos.y pos.x

  ||| Draw text at the given point
  export
  showTextAt : Pos -> String -> IO ()
  showTextAt pos x = do
    moveTo pos
    putStr x

  ||| Clear the contents of the screen via ANSI codes.
  export
  clearScreen : IO ()
  clearScreen = putStr $ eraseScreen All

  ||| Show the cursor
  export
  showCursor : IO ()
  showCursor = putStr "\ESC[?25h"

  ||| Hide cursor
  export
  hideCursor : IO ()
  hideCursor = putStr "\ESC[?25l"

  export
  saveCursor : IO ()
  saveCursor = putStr "\ESC7"

  export
  restoreCursor : IO ()
  restoreCursor = putStr "\ESC8"

  export
  reverseVideo : IO ()
  reverseVideo = putStr "\ESC[7m"

  export
  sgr : List SGR -> IO ()
  sgr = putStr . escapeSGR

||| A view is a high-level UI component.
|||
||| - It wraps an inner value, its state.
||| - It knows how to size itself, for layout purposes.
||| - It can draw itself to the screen
||| - It can update its state in response to events.
public export
interface View state where
  ||| The value of this view type.
  0 Value : Type
  Value = state

  ||| Get the current value of the view
  |||
  ||| This is so a stateful widget can project its inner value.  an
  ||| inner value.
  value : state -> Value

  ||| Calculate the "requested" size
  size  : state -> Area

  ||| Draw the view into the given screen rectangle.
  paint : Rect -> state -> IO ()

  ||| Possibly update our state in response to a key press.
  |||
  ||| The default implementation is a no-op. Override this for
  ||| stateful view.
  handle : Key -> state -> state
  handle _ s = s


||| Associated definitions `View`.
namespace View

  ||| Implement `View` for `()` as a no-op
  export
  View () where
    size  _   = MkArea 0 0
    paint _ _ = pure ()
    value     = id

  ||| Any type implementing `Show` is automatically a (non-interative)
  ||| view.
  export
  Show a => View a where
    Value = a
    value = id
    size s = MkArea (length (show s)) 1
    paint r s = showTextAt r.nw (show s)

  ||| In implementing `View` for all `Show` types, we have
  ||| inadvertently made it ambigious what to do when we use a string
  ||| as a view. This alternative, named implementation draws the
  ||| string directly to the screen.
  export
  [string] View String where
    value = id
    size s = MkArea (length s) 1
    paint r = showTextAt r.nw


||| A heterogenous list of views.
|||
||| XXX: This type may be unnecessary. Consider replacing it with
||| HVect.  Alternatively, this might become part of a `Container`
||| interface.
public export
data ViewList : Type -> Type where
  End   : ViewList ()
  Field : String -> View a => a -> ViewList b -> ViewList (a, b)


||| Associated definitions for `ViewList`
namespace ViewList
  ||| Project the values out of the ViewList as a tuple.
  export
  values : ViewList ty -> ty
  values End = ()
  values (Field _ x y) = (x, values y)

  ||| Project the labels out of the ViewList as a list of strings.
  export
  labels : ViewList _ -> List String
  labels End = []
  labels (Field s _ y) = s :: labels y

  ||| The size of the widest label in the the view list.
  export
  labelSplit : ViewList _ -> Nat
  labelSplit l = foldl max 0 $ length <$> labels l

  ||| Calculate the natural size of this view list.
  export
  sizeVertical : ViewList _ -> Area
  sizeVertical l =
    let MkArea width height = rec l
    in MkArea (width + labelSplit l) height
    where
      rec : ViewList _ -> Area
      rec End = MkArea 0 0
      rec (Field _ x y) = vunion (size x) (rec y)

  ||| Render the view-list along a vertical axis.
  export
  paintVertical : Nat -> Rect -> ViewList _ -> IO ()
  paintVertical focus r l = loop 0 (labelSplit l) r l
    where
      loop : Nat -> Nat -> Rect -> ViewList _ -> IO ()
      loop _ _ _ End = pure ()
      loop i split r (Field s x y) = do
        let (left, right) = hsplit r (split + 4)
        if i == focus
          then paint left (bolden s)
          else paint @{string} left s
        let vsep = if i == focus then ">" else "|"
        showTextAt (MkPos (split + 2) r.n) "|"
        if i == focus
          then do
            saveCursor
            reverseVideo
            paint right x
            restoreCursor
          else paint right x
        loop (S i) split (snd $ vsplit r ((height (size x)) + 1) ) y

  ||| Dispatch keyboard input to the currently-focused subview.
  export
  handleNth : Nat -> Key -> ViewList ty -> ViewList ty
  handleNth _     _ End           = End
  handleNth Z     k (Field s x y) = Field s (handle k x) y
  handleNth (S n) k (Field s x y) = Field s x (handleNth n k y)

  ||| Return the length of the viewlist.
  export
  length : ViewList _ -> Nat
  length End = Z
  length (Field _ _ y) = S (length y)


||| An interactive view which represents an exclusive choice.
|||
||| XXX: rename me
||| this should be a "spinner" or "chooser", or "combo"
record Menu a where
  constructor MkMenu
  n       : Nat
  choices : Vect n a
  choice  : Fin  n

||| Associated definitions for `Menu`
namespace Menu

  ||| A unicode up arrow
  upArrow : String
  upArrow = "⬆"

  ||| A unicode down arrow
  downArrow : String
  downArrow = "⬇"

  ||| A unicode up/down arrow
  upDownArrow : String
  upDownArrow = "⬍"

  ||| Show the arrow indicator most appropriate for the given as a
  ||| hint to the user which keys will be effective.
  |||
  ||| - 0     : down arrow
  ||| - last  : the up arrow
  ||| - other : the up-down arrow.
  arrowForIndex : {k : Nat} -> Fin k -> String
  arrowForIndex FZ     = downArrow
  arrowForIndex (FS n) =
    if FS n == last
    then upArrow
    else upDownArrow

  ||| View implementation for Menu
  |||
  ||| Up / Down is used to cycle through alternatives.
  ||| TBD: filter options based on text typed.
  export
  View a => View (Menu a) where
    Value = Value {state = a}
    value self = value $ index self.choice self.choices

    size self =
      let width = foldl max 0 $ map (width . size) self.choices
      in MkArea (width + 2) $ height $ size $ index self.choice self.choices

    paint rect self = do
      paint (snd $ hsplit rect 2) (index self.choice self.choices)
      showTextAt rect.nw (arrowForIndex {k = self.n} self.choice)

    handle Up   state = { choice := pred state.choice } state
    handle Down state = { choice := finS state.choice } state
    handle _    state = state

  ||| Construct a menu from a vector of views
  export
  menu : {k : Nat} -> View a => Vect (S k) a -> Menu a
  menu {k} choices = MkMenu (S k) choices (natToFinLt 0)


||| A form displays a set of views, each with a string label.
|||
||| Only one sub-view has focus, and user input is routed to this
||| sub-view.
record Form ty where
  constructor MkForm
  views : ViewList ty
  choice : Nat

||| Associated definitions for `Form`.
namespace Form
  ||| Increment the choice by one.
  export
  nextChoice : Form ty -> Form ty
  nextChoice self = {
    choice := (self.choice + 1) `mod` (length self.views)
  } self

  ||| The View implementation for form renders each labeled sub-view
  ||| vertically.
  |||
  ||| The labels are left-justified, while the sub-views are
  ||| left-justified, and aligned relative to the widest label in the
  ||| form.
  |||
  ||| Only one sub-view has focus. Tab is used to move focus to the
  ||| next form field.
  View (Form ty) where
    Value = ty
    value self = values self.views

    size self = sizeVertical self.views

    paint rect self = do
      paintVertical self.choice rect self.views
      moveTo rect.sw

    handle Tab self = nextChoice self
    handle key self = {
      views := handleNth self.choice key self.views
    } self


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
runView : View state => state -> IO (Value {state = state})
runView init = do
  let window = r80x24 -- XXX: get real window size
  result <- runTUI wrapView (paint window) init
  pure $ value result
where
  wrapView : Key -> state -> Maybe state
  wrapView k s = Just $ handle k s



--- tests

testMenu : Menu String
testMenu = menu ["foo", "bar", "baz"]

testViewList : ViewList (Menu String, (Menu String, ()))
testViewList = Field "F1" testMenu $ Field "Long name" testMenu End

testForm : Form (Menu String, (Menu String, ()))
testForm = MkForm {
  views = testViewList,
  choice = 0
}

partial export
test : IO ()
test = do
  _ <- runView testForm
  putStrLn ""
