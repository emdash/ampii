module Inventory

import Data.List1
import Data.Vect
import Data.HashMap
import JSON.Derive
import Data.String

import Barcode
import Food
import Measures


%default total
%language ElabReflection


data Message
  = Key String
  | ScaleReady
  | Weight Double String
  | Quit
%runElab derive "Message" [Show,Eq,ToJSON,FromJSON]


||| A single item in the inventory
record Item where
  barcode: Barcode
  name:    String
  init_wt: Weight
  last_wt: Weight
  net_wt:  Weight
%runElab derive "Item" [Show,Eq,Ord,FromJSON,ToJSON]


||| The global state for the `inventory` subcommand
record State where
  constructor MkState
  curBarcode: Maybe String
  curWeight:  Maybe (Weight, Double)
  items:      HashMap Barcode Item

||| Print a human-readable representation of the current state.
Show State where
  show state = 
    "Barcode: " ++ (show state.curBarcode)     ++ "\n" ++
    "Weight:  " ++ (show state.curWeight)      ++ "\n" ++
    "Items:   " ++ (show (length (keys state.items))) ++ "\n"

||| Return a blank state
empty : State
empty = MkState Nothing Nothing empty

||| Handle receiving a key press
onKey : State -> String -> State
onKey state chars = case state.curBarcode of
  Nothing => {curBarcode := Just chars} state
  Just c  => {curBarcode := Just (c ++ chars)} state

||| Handle receiving a `ScaleReady` mesage
onReady : State -> State
onReady state = {curWeight := Nothing} state

||| Handle receiving a `Weight` message
onWeight : State -> Double -> String -> State
onWeight state w "oz" = {curWeight := Just (Oz,   w)} state
onWeight state w "g"  = {curWeight := Just (Gram, w)} state
onWeight state w "lb" = {curWeight := Just (Lb,   w)} state
onWeight state _ _    = state

||| Update state in response to a single mesage
eval : State -> Message -> Maybe State
eval state (Key c)      = Just $ onKey    state c
eval state ScaleReady   = Just $ onReady  state
eval state (Weight w u) = Just $ onWeight state w u
eval state Quit         = Nothing


||| Process messages from stdin and fold into the state.
covering export
run : State -> IO ()
run state = do
  putStrLn (show state)
  line <- getLine
  putStrLn $ "got line: " ++ (show line)
  case decode {a = Message} line of
    (Left  err) => putStrLn $ "Invalid Message: " ++ line
    (Right msg) => case eval state msg of
      Nothing    => putStrLn "Exiting..."
      Just state => run state


||| Entry point for the `inventory` subcommand.
covering export
main : List String -> IO ()
main _ = run empty
