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


||| Abstract key value
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
%runElab derive "Key" [Ord, Eq, Show]


||| Wrapper state which tracks escape sequence state.
data EscState state
  = HaveEsc (SnocList Char) state
  | Default state

unwrapEsc : EscState state -> state
unwrapEsc (HaveEsc _ s) = s
unwrapEsc (Default   s) = s

Show s => Show (EscState s) where
  show = show . unwrapEsc

||| Decode well-known escape sequences into abstract keys.
|||
||| In particular, we handle the cursor keys.
decodeEsc : SnocList Char -> Maybe (Maybe Key)
decodeEsc [<]          = Just Nothing
decodeEsc [< '[']      = Just Nothing
decodeEsc [< '[', 'C'] = Just $ Just Right
decodeEsc [< '[', 'D'] = Just $ Just Left
decodeEsc [< '[', 'A'] = Just $ Just Up
decodeEsc [< '[', 'B'] = Just $ Just Down
decodeEsc _            = Nothing


||| Interpret console escape sequences as keys.
|||
||| Note: this falls down if the user presses the escape key, because
||| we can't tell the difference between the key press and the start
||| of an escape sequence. It'd be better to use ncurses.
interpretEsc
  :  (Key -> state -> Maybe state)
  -> Char
  -> EscState state
  -> Maybe (EscState state)
interpretEsc f c (HaveEsc esc s) = case (decodeEsc $ esc :< c) of
  Just Nothing    => Just $ HaveEsc (esc :< c) s
  Just (Just key) => Default <$> (f key s)
  Nothing         => Just $ Default s
interpretEsc f '\ESC' (Default s) = Just $ HaveEsc [<] s
interpretEsc f '\DEL' (Default s) = map Default $ f Delete    s
interpretEsc f '\n'   (Default s) = map Default $ f Enter     s
interpretEsc f '\t'   (Default s) = map Default $ f Tab       s
interpretEsc f c      (Default s) = map Default $ f (Alpha c) s

||| Truncate a string to the given length.
truncateTo : Nat -> String -> String
truncateTo n s = pack $ take n $ unpack s

||| Format a string into an exact width
|||
||| Pad if length is shorter than width, truncate if longer.
frameString : Nat -> String -> String
frameString w s =
  let l = length s
  in case (l < length s) of
    True  => padRight w ' ' s
    False => truncateTo w s

||| Find the difference of two natural numbers regardless of ordering.
diff : Nat -> Nat -> Nat
diff a b = case a < b of
  True  => b `minus` a
  False => a `minus` b

||| The location of a character cell in the terminal
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
(+) : Pos -> Area -> Pos
(+) (MkPos x y) (MkArea w h) = MkPos (x + w) (y + h)

||| Subtragcting two points yields an area.
(-) : Pos -> Pos -> Area
(-) a b = MkArea (a.x `diff` b.x) (a.y `diff` b.y)

clearScreen : IO ()
clearScreen = putStr $ eraseScreen All

moveTo : Pos -> IO ()
moveTo pos = putStr $ cursorMove pos.y pos.x

showTextAt : Pos -> String -> IO ()
showTextAt pos x = do
  moveTo pos
  putStr x

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

||| A rectangular screen region
record Rect where
  constructor MkRect
  pos  : Pos
  size : Area
%runElab derive "Rect" [Eq, Ord, Show]

namespace Rect
  export
  (.nw) : Rect -> Pos
  (.nw) b = b.pos

  export
  (.ne) : Rect -> Pos
  (.ne) b = b.pos + MkArea b.size.width 0

  export
  (.se) : Rect -> Pos
  (.se) b = b.pos + b.size

  export
  (.sw) : Rect -> Pos
  (.sw) b = b.pos + MkArea 0 b.size.height

  export
  (.w) : Rect -> Nat
  (.w) b = b.pos.x

  export
  (.e) : Rect -> Nat
  (.e) b = b.pos.x + b.size.width

  export
  (.n) : Rect -> Nat
  (.n) b = b.pos.y

  export
  (.s) : Rect -> Nat
  (.s) b = b.pos.y + b.size.height

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

  ||| The bounding box fully containing both rectangles.
  export
  union : Rect -> Rect -> Rect
  union a b =
    let
      tl = min a.nw b.nw
      br = min a.se b.se
    in
      fromPoints tl br

  export
  Semigroup Rect where
    (<+>) = union


||| A view is something which can render itself to the screen
public export
interface View state where
  0 Value : Type
  Value = state

  ||| Get the current value of the view
  value : state -> Value

  ||| Calculate the "requested" size
  size  : state -> Area
  ||| Draw the view to the screen
  paint : Rect -> state -> IO ()

  ||| Default event handler does nothing
  handle : Key -> state -> state
  handle _ s = s


||| The trivial view
export
View () where
  size _    = MkArea 0 0
  paint _ _ = pure ()
  value     = id

Show a => View a where
  Value = a
  value = id
  size s = MkArea (length (show s)) 1
  paint r s = showTextAt r.nw (show s)

[string]
View String where
  value = id
  size s = MkArea (length s) 1
  paint r = showTextAt r.nw


pred : Fin n -> Fin n
pred FZ = FZ
pred (FS k) = weaken k

record Menu a where
  constructor MkMenu
  n       : Nat
  choices : Vect n a
  choice  : Fin  n

export
View a => View (Menu a) where
  Value = Value {state = a}
  value self = value $ index self.choice self.choices

  size self =
    let width = foldl max 0 $ map (width . size) self.choices
    in MkArea width 1

  paint rect self = paint rect (index self.choice self.choices)

  handle Up   state = { choice := pred state.choice } state
  handle Down state = { choice := finS state.choice } state
  handle _    state = state

namespace Menu
  export
  menu : {k : Nat} -> View a => Vect (S k) a -> Menu a
  menu {k} choices = MkMenu (S k) choices (natToFinLt 0)


data ViewList : Type -> Type where
  End   : ViewList ()
  Field : String -> View a => a -> ViewList b -> ViewList (a, b)

namespace ViewList
  export
  values : ViewList ty -> ty
  values End = ()
  values (Field _ x y) = (x, values y)

  export
  labels : ViewList _ -> List String
  labels End = []
  labels (Field s _ y) = s :: labels y

  export
  labelSplit : ViewList _ -> Nat
  labelSplit l = foldl max 0 $ length <$> labels l

  export
  sizeVertical : ViewList _ -> Area
  sizeVertical l =
    let MkArea width height = rec l
    in MkArea (width + labelSplit l) height
    where
      rec : ViewList _ -> Area
      rec End = MkArea 0 0
      rec (Field _ x y) = vunion (size x) (rec y)

  export
  paintVertical : Nat -> Rect -> ViewList _ -> IO ()
  paintVertical focus r l =
    rec 0 (labelSplit l) r l
    where
      rec : Nat -> Nat -> Rect -> ViewList _ -> IO ()
      rec _ _ _ End = pure ()
      rec i split r (Field s x y) = do
        let (left, right) = hsplit r (split + 4)
        if i == focus 
          then paint left (bolden s)
          else paint @{string} left s
        let vsep = if i == focus then ">" else "|"
        showTextAt (MkPos (split + 2) r.n) vsep
        paint right x
        rec (S i) split (snd $ vsplit r ((height (size x)) + 1) ) y

  export
  handleNth : Nat -> Key -> ViewList ty -> ViewList ty
  handleNth _     _ End           = End
  handleNth Z     k (Field s x y) = Field s (handle k x) y
  handleNth (S n) k (Field s x y) = Field s x (handleNth n k y)

  export
  length : ViewList _ -> Nat
  length End = Z
  length (Field _ _ y) = S (length y)

record Form ty where
  constructor MkForm
  views : ViewList ty
  choice : Nat


nextChoice : Form ty -> Form ty
nextChoice self = {
  choice := (self.choice + 1) `mod` (length self.views)
} self


View (Form ty) where
  Value = ty
  value self = values self.views

  size self = sizeVertical self.views

  paint rect self = paintVertical self.choice rect self.views

  handle Tab self = nextChoice self
  handle key self = { views := handleNth self.choice key self.views } self




||| TUI application mainloop.
|||
||| This takes care of setting and unsetting raw mode on the terminal,
||| interpreting escape sequences into high-level keystrokes.
partial
runRaw
  :  (Char -> state -> Maybe state)
  -> (state -> IO Builtin.Unit)
  -> state
  -> IO state
runRaw handler render init = do
  -- default SigINT handler doesn't clean up raw mode
  Right _ <- collectSignal SigINT | Left err => die "signal"
  -- tty doesn't enter raw mode til it sees newline
  putStrLn ""
  -- run the inner loop
  withRawMode (\x => die "don't care") $ \x => loop init
  where
    loop : state -> IO state
    loop s = do
      -- repaint the screen with the current state
      clearScreen
      moveTo (MkPos 0 0)
      render s
      -- loop until sigint is raised
      sig <- handleNextCollectedSignal
      case sig of
        Nothing => do
          -- no signals raised, wait for handle next key press.
          c <- getChar
          case handler c s of
            -- we are done, quit
            Nothing => pure s
            -- we got a new state, continue to next iteration
            Just s  => loop s
        -- user hit ctrl-c, quit
        Just SigINT => pure s
        -- this shouldn't happen but clean up properly and exit with a
        -- code.
        Just _      => idris_crash "unhandled signal"


||| Run a TUI app, starting with the given initial state.
partial export
runTUI
  :  (Key -> state -> Maybe state)
  -> (state -> IO ())
  -> state
  -> IO state
runTUI handler render init = do
  final <- runRaw (interpretEsc handler) (render . unwrapEsc) (Default init)
  pure $ unwrapEsc final

r80x24 : Rect
r80x24 = MkRect (MkPos 0 0) (MkArea 80 24)

partial export
runView : View state => state -> IO (Value {state = state})
runView init = do
  let window = r80x24 -- XXX: get real window size
  result <- runTUI wrapView (paint window) init
  pure $ value result
  where wrapView : Key -> state -> Maybe state
        wrapView k s = Just $ handle k s


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
  result <- runView testForm
  putStrLn ""
