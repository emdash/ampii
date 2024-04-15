||| Minimalist terminal UI framework.
|||
||| Entry points for running TUI applications.
module TUI.MainLoop

import TUI.View
import System
import System.File
import System.Signal
import Data.Vect
import Data.Vect.Quantifiers
import Util


%default total


||| Low-level TUI application mainloop.
|||
||| Manages terminal state, handles OS-level signals, and receives
||| keyboard events from STDIN.
|||
||| Use this entry point if you do not want escape-sequence decoding.
|||
||| SIGINT is interpreted as *Cancel*, meaning the initial state is
||| returned, discarding the user's changes.
|||
||| Handler is called to process input events. It main job is to
||| compute the next state, but it runs in IO so that your program can
||| take arbitrary actions in response to user input.
export covering
runRaw
  :  (handler : Char -> state -> IO (Maybe state))
  -> (render  : state -> IO Builtin.Unit)
  -> (init    : state)
  -> IO state
runRaw handler render init = do
  -- default SigINT handler doesn't clean up raw mode, so we need to
  -- handle it explicitly and make sure to clean up.
  Right _ <- collectSignal SigINT
           | Left err => die "couldn't trap SigINT"

  -- run mainloop
  altScreen True
  hideCursor
  saveCursor
  ret <- withRawMode err (loop init)
  cleanup
  pure ret
where
  -- restore terminal state as best we can
  cleanup : IO ()
  cleanup = do
    clearScreen
    restoreCursor
    showCursor
    altScreen False

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
             | Just SigINT => pure init
             | Just _      => die "unexpected signal"

    -- handle next key press
    case !(handler !getChar s) of
      Nothing => pure s -- we are done, quit
      Just s  => loop s () -- go to next iteration.

||| Run a raw TUI application, decoding input escape sequences.
|||
||| Use this function if you want escape sequence decoding, but do not
||| want to use the view abstraction for rendering screen contents.
covering export
runTUI
  :  (handler : Key -> state -> IO (Maybe state))
  -> (render  : state -> IO ())
  -> (init    : state)
  -> IO state
runTUI handler render init = do
  ret <- runRaw (interpretEsc handler) (render . unwrapEsc) (wrapEsc init)
  pure $ unwrapEsc ret

||| Run a top-level View.
|||
||| Use this entry point if you want to use the `View` abstraction.
|||
||| This will run until the top-level view gives up its focus.
covering export
runView : View state => state -> IO state
runView init = do
  result <- runTUI wrapView (paint Focused !(screen)) init
  pure result
where
  wrapView : Key -> state -> IO (Maybe state)
  wrapView k s = pure $ case handle k s of
    Update s    => Just s
    FocusParent => Nothing
    FocusNext   => Just s
