||| This module manages a personal food inventory as a subdirectory of
||| JSON-encoded files.
module Inventory


import Data.List1
import Data.Vect
import Data.SortedMap
import JSON.Derive
import Data.String
import System.File
import System.Directory


import Barcode
import Food
import Measures


%default total
%language ElabReflection


||| These are the high-level operations on the inventory
data Command
  = Scan  Barcode
  | Name  String
  | Weigh Weight Double
  | Quit
%runElab derive "Command" [Show,Eq,ToJSON,FromJSON]


||| A single item in the inventory
record Item where
  constructor MkItem
  barcode: Barcode
  name:    String
  init_wt: (Weight, Double)
  net_wt:  Maybe (Weight, Double)
  last_wt: Maybe (Weight, Double)
%runElab derive "Item" [Show,Eq,Ord,FromJSON,ToJSON]

||| Create a new item
newItem : Barcode -> String -> (Weight, Double) -> Item
newItem bc name w = MkItem bc name w Nothing Nothing

||| Get an item's remaining weight
remaining : Item -> Maybe (Weight, Double)
remaining item = case item.net_wt of
  Nothing => item.last_wt
  Just (unit, weight) => case item.last_wt of
    Nothing => Nothing
    Just lw => Just (unit, weight - (snd lw))

||| Store the item's contents to the given directory.
|||
||| The filename is derived from the item's barcode.
save : String -> Item -> IO ()
save path item = do
  let filename = path ++ "/" ++ show item.barcode ++ ".json"
  Right _ <- writeFile filename $ encode item
          | Left err => putStrLn $ show err
  pure ()


||| The abstract state of the inventory process
|||
||| The inventory starts off in the `Ready` state. After scanning a
||| barcode, we look for the item in the database. If found, we enter
||| the `Updating` state, otherwise we enter the `Creating` state.
|||
||| The `Updating` state holds a valid Item, and allows updating its
||| current weight. The `Creating` state holds a partial item.
data State
  = Ready
  | Err      String
  | Updating Item
  | Creating Barcode (Maybe String) (Maybe (Weight, Double))
%runElab derive "State" [Show]

||| Common code path for setName and setWeight
|||
||| The basica idea is that we have a partial record, where some
||| values are Maybe. If we have everything, then we advance to the
||| `Updating` state, otherwise we continue collecting fields.
helper : Barcode -> Maybe String -> Maybe (Weight, Double) -> State
helper bc (Just name) (Just weight) = Updating $ newItem bc name weight
helper bc name        weight        = Creating bc name weight

||| Update the name of the current item.
setName : String -> State -> Either String State
setName name (Updating item)        = Right $ Updating $ {name := name} item
setName name (Creating bc _ weight) = Right $ helper bc (Just name) weight
setName _    _                      = Left "No Barcode"

||| Update the weight of the current item.
setWeight : (Weight, Double) -> State -> Either String State
setWeight w (Updating item)         = Right $ Updating $ {last_wt := Just w} item
setWeight w (Creating bc name _)    = Right $ helper bc name $ Just w
setWeight _ _                       = Left "No Barcode"


||| Manages a subdirectory with a food item per barcode
|||
||| XXX: barcodes are assumed to be unique
record Inventory where
  constructor MkInventory
  path: String
  state: State
%runElab derive "Inventory" [Show]

||| Create a new item in the inventory
createItem : Barcode -> Inventory -> Inventory
createItem bc inv = { state := Creating bc Nothing Nothing } inv

||| Open the inventory database at the given path
openInventory   : String -> IO (Either String Inventory)
openInventory path = do
  Right dir <- openDir path
             | Left _ => pure $ Left "Invalid Path"
  closeDir dir
  pure $ Right $ MkInventory path Ready

||| Respond to receiving a barcode from the user
|||
||| If the item is already in the inventory, then we update the item's
||| record. Otherwise we enter the mode for soliciting new items from
||| the user.
covering
scan : Barcode -> Inventory -> IO Inventory
scan bc inv = do
  let path = inv.path ++ "/" ++ show bc ++ ".json"
  case inv.state of
    Updating item => do
      putStrLn $ "Saving:" ++ path
      save inv.path item
    _ => pure ()
  Right contents <- readFile path | Left err => pure $ createItem bc inv
  case decode {a = Item} contents of
    Left err   => pure $ createItem bc inv
    Right item => pure $ { state := Updating item } inv

||| Apply the given weight to the current item, if possible
weigh : (Weight, Double) -> Inventory -> Inventory
weigh w inv = case setWeight w inv.state of
  Left err => inv
  Right state => { state := state } inv

||| Apply the given name to the current item, if possible
name : String -> Inventory -> Inventory
name n inv = case setName n inv.state of
  Left err => inv
  Right state => { state := state } inv

||| Handle Command Dispatch
covering
eval : Inventory -> Command -> IO (Maybe Inventory)
eval inv (Scan bc) = do
  inv <- scan bc inv
  pure $ Just inv
eval inv (Name n)    = pure $ Just $ name n inv
eval inv (Weigh u w) = pure $ Just $ weigh (u, w) inv
eval inv Quit        = pure Nothing


{- Entry Point ---------------------------------------------------------------}


||| Process messages from stdin and fold into the state.
covering export
run : Inventory -> IO ()
run inv = do
  putStrLn (show inv)
  line <- getLine
  putStrLn $ "got line: " ++ (show line)
  case decode {a = Command} line of
    (Left  err) => do
      putStrLn $ "Invalid Command: " ++ line
      run inv
    (Right msg) => case !(eval inv msg) of
      Nothing => pure ()
      Just inv => run inv

||| Entry point for the `inventory` subcommand.
covering export
main : List String -> IO ()
main (path :: rest) = do
  Right inv <- openInventory path
            | Left err => putStrLn err
  run inv
main _ = do
  putStrLn "No path given"
