||| Minimalist terminal UI framework.
|||
||| Extended support for ANSI escape sequenecs. Some or all of this
||| could be contributed to the upstream `ansi` repo, after a good
||| refactoring.
|||
||| These routines are slightly higher-level than that provided by the
||| ANSI library. In particular, they use types from `TUI.Geometry`
||| rather than passing coordinates directly. This avoids confusion
||| around the row-major coordinates used by terminals.
module TUI.Painting


import public Control.ANSI
import public TUI.Geometry
import        Data.String


%default total


||| Functions and related to putting text on the screen
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

||| Draw a single character at the given point.
export
showCharAt : Pos -> Char -> IO ()
showCharAt pos x = showTextAt pos (singleton x)

||| Clear the contents of the screen via ANSI codes.
export
clearScreen : IO ()
clearScreen = putStr $ eraseScreen All

||| Switch into or out of the alternate screen buffer
export
altScreen : Bool -> IO ()
altScreen True  = putStr $ "\ESC[?1049h"
altScreen False = putStr $ "\ESC[?1049l"

||| Show the cursor
export
showCursor : IO ()
showCursor = putStr "\ESC[?25h"

||| Hide cursor
export
hideCursor : IO ()
hideCursor = putStr "\ESC[?25l"

||| Save the cursor state and position.
|||
||| The standard only supports one level of save / restore. This
||| should be called once at application start.
export
saveCursor : IO ()
saveCursor = putStr "\ESC7"

||| Save the cursor state and position.
|||
||| The standard only supports one level of save / restore. This
||| should be called once at application end.
export
restoreCursor : IO ()
restoreCursor = putStr "\ESC8"

||| This attribute isn't part of the ANSI library in contrib, but is
||| arguably more useful than setting explicit colors.
export
reverseVideo : IO ()
reverseVideo = putStr "\ESC[7m"

||| Undoes the above
export
unreverseVideo : IO ()
unreverseVideo = putStr "\ESC[27m"

||| effectful version for setting arbitrary SGR attributes
export
sgr : List SGR -> IO ()
sgr = putStr . escapeSGR

||| Symbolic type for box drawing characters
public export
data BoxChar
  = NW
  | NE
  | SW
  | SE
  | H
  | V

||| Draw the corresponding box character
export
boxChar : Pos -> BoxChar -> IO ()
boxChar pos NW = showCharAt pos $ cast 0x250C
boxChar pos NE = showCharAt pos $ cast 0x2510
boxChar pos SW = showCharAt pos $ cast 0x2514
boxChar pos SE = showCharAt pos $ cast 0x2518
boxChar pos H  = showCharAt pos $ cast 0x2500
boxChar pos V  = showCharAt pos $ cast 0x2502

||| Draw a horizontal line
export
hline : Pos -> Nat -> IO ()
hline pos@(MkPos x y) width = do
  boxChar pos H
  case width of
    Z   => pure ()
    S n => hline (MkPos (S x) y) n

||| Draw a vertical line
export
vline : Pos -> Nat -> IO ()
vline pos@(MkPos x y) height = do
  boxChar pos V
  case height of
    Z   => pure ()
    S n => vline (MkPos x (S y)) n

||| Draw a box around the given rectangle
|||
||| Use with `shrink` or `inset` to layout contents within the frame.
export
box : Rect -> IO ()
box r = do
  -- draw the lines at full size
  hline r.nw r.size.width
  hline r.sw r.size.width
  vline r.nw r.size.height
  vline r.ne r.size.height
  -- paint over with the corners
  boxChar r.nw NW
  boxChar r.ne NE
  boxChar r.sw SW
  boxChar r.se SE
