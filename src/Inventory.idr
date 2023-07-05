module Inventory

import Data.Vect
import Data.HashMap
import JSON.Derive
import Measures
import Food


%default total
%language ElabReflection


data Message
  = Key String
  | ScaleReady
  | Weight Double String
  | Quit

%runElab derive "Message" [Show,Eq,ToJSON,FromJSON]
  
data Barcode 
  = EAN13 (Vect 13 Char)
  | UPC   (Vect 11 Char)
  
record Item where
  barcode: Barcode
  name:    String
  netWt:   Weight
  curWt:   Weight
  
record State where
  constructor MkState
  curBarcode: Maybe String
  curWeight:  Maybe (Weight, Double)
  items:      HashMap String Item

  
Show State where
  show state = 
    "Barcode: " ++ (show state.curBarcode)     ++ "\n" ++
    "Weight:  " ++ (show state.curWeight)      ++ "\n" ++
    "Items:   " ++ (show (length (keys state.items))) ++ "\n"

  
empty : State
empty = MkState Nothing Nothing empty
    

onKey : State -> String -> State
onKey state chars = case state.curBarcode of
  Nothing => {curBarcode := Just chars} state
  Just c  => {curBarcode := Just (c ++ chars)} state


onReady : State -> State
onReady state = {curWeight := Nothing} state


onWeight : State -> Double -> String -> State
onWeight state w "oz" = {curWeight := Just (Oz,   w)} state
onWeight state w "g"  = {curWeight := Just (Gram, w)} state
onWeight state w "lb" = {curWeight := Just (Lb,   w)} state
onWeight state _ _    = state


eval : State -> Message -> Maybe State
eval state (Key c)      = Just $ onKey    state c
eval state ScaleReady   = Just $ onReady  state
eval state (Weight w u) = Just $ onWeight state w u
eval state Quit         = Nothing


covering export
run : State -> IO ()
run state = do
  putStrLn (show state)
  line <- getLine
  putStrLn $ "got line: " ++ (show line)
  --Right msg <- decode line | Left err => ?hole
  case decode {a = Message} line of
    (Left  err) => putStrLn $ "Invalid Message: " ++ line
    (Right msg) => case eval state msg of
      Nothing    => putStrLn "Exiting..."
      Just state => run state


covering export
main : List String -> IO ()
main _ = run empty
