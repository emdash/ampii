||| Minimalist terminal UI framework.
|||
||| I can't get ncurses-idris working, so I'm rolling this pure-idris
||| replacement.
module TUI


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
%runElab derive "Key" [Ord, Eq, Show]


||| Wrapper state which tracks escape sequence state.
data EscState state
  = HaveEsc (SnocList Char) state
  | Default state
%runElab derive "EscState" [Show]


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
  :  Show state
  => (Key -> state -> Maybe state)
  -> Char
  -> EscState state
  -> Maybe (EscState state)
interpretEsc f c (HaveEsc esc s) = case (decodeEsc $ esc :< c) of
  Just Nothing    => Just $ HaveEsc (esc :< c) s
  Just (Just key) => Default <$> (f key s)
  Nothing         => Just $ Default s
interpretEsc f '\ESC' (Default s) = Just $ HaveEsc [<] s
interpretEsc f '\DEL' (Default s) = map Default $ f Delete s
interpretEsc f c      (Default s) = map Default $ f (Alpha c) s


||| Run a raw-mode TUI app
partial export
runRaw
  :  Show state
  => (Char -> state -> Maybe state)
  -> state
  -> IO ()
runRaw handler init = do
  -- default SigINT handler doesn't clean up raw mode
  Right _ <- collectSignal SigINT | Left err => die "signal"
  -- tty doesn't enter raw mode til it sees newline
  putStrLn ""
  -- run the inner loop
  status <- withRawMode (\x => die "don't care") $ \x => loop init
  -- print newline and exit
  putStrLn ""
  exitWith status
  where
    loop : state -> IO ExitCode
    loop s = do
      -- repaint the screen with the current state
      putStr $ eraseScreen All
      putStr $ cursorMove 0 0
      putStrLn $ show s
      -- loop until sigint is raised
      sig <- handleNextCollectedSignal
      case sig of
        Nothing => do
          -- update state in response to each char from stdin
          c <- getChar
          case handler c s of
            -- we are done, quit
            Nothing => pure ExitSuccess
            -- we got a new state, continue to next iteration
            Just s  => loop s
        -- user hit ctrl-c, quit
        Just SigINT => pure ExitSuccess
        -- this shouldn't happen but clean up properly and exit with a
        -- code.
        Just _      => pure $ ExitFailure 1


partial export
runTUI
  :  Show state
  => (Key -> state -> Maybe state)
  -> state
  -> IO ()
runTUI handler init = runRaw (interpretEsc handler) (Default init)
