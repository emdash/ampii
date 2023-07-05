module Inventory

import Data.Vect
import Language.JSON
import Data.HashMap

import Measures
import Food


%default total


data Message
  = KeyPress String
  | ScaleReady
  | Weight Double String
  | Quit
  
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


  
decode : Maybe JSON -> Maybe Message
decode Nothing    = Nothing
decode (Just msg) = do
  case msg of
    JObject [("type",   JString "quit")]              => Just $ Quit
    JObject [("type",   JString "key"), 
             ("chars",  JString char)]                => Just $ KeyPress char
    JObject [("type",   JString "scale_ready")]       => Just $ ScaleReady
    JObject [("type",   JString "weight"), 
             ("weight", JNumber w),
             ("unit",   JString unit)]                => Just $ Weight w unit
    _                                                 => Nothing
    

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


eval : State -> Message -> State
eval state (KeyPress c) = onKey    state c
eval state ScaleReady   = onReady  state
eval state (Weight w u) = onWeight state w u
eval state Quit         = ?unreachable


covering export
run : State -> IO ()
run state = do
  putStrLn (show state)
  line <- getLine
  putStrLn $ "got line: " ++ (show line)
  case decode (parse line) of
    Nothing   => putStrLn "Invalid message"
    Just Quit => putStrLn "Exiting..."
    Just msg  => run (eval state msg)


covering export
main : List String -> IO ()
main _ = run empty
