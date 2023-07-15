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
import System.Concurrency


import Barcode
import Food
import Measures
import USBScale


%default total
%language ElabReflection


||| These are the high-level operations on the inventory
data Command
  = Scan  Barcode
  | Name  String
  | Weigh (Weight, Double)
  | NetWt (Weight, Double)
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

||| Update the net weight of the current item.
setNetWt : (Weight, Double) -> State -> Either String State
setNetWt w (Updating item)         = Right $ Updating $ {net_wt := Just w} item
setNetWt w (Creating bc name _)    = Left "Weigh Item First"
setNetWt _ _                       = Left "No Barcode"


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

||| Apply the given net weight to the current item, if possible
netwt : (Weight, Double) -> Inventory -> Inventory
netwt w inv = case setNetWt w inv.state of
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
eval inv (Name n)  = pure $ Just $ name n inv
eval inv (Weigh w) = pure $ Just $ weigh w inv
eval inv (NetWt w) = pure $ Just $ netwt w inv
eval inv Quit      = case inv.state of
  Updating item => do
    save inv.path item
    pure Nothing
  _ => pure Nothing

parseNetWeight : String -> Maybe (Weight, Double)
parseNetWeight str =
  let
    (decimal, tail) := break (\x => not (isDigit x || x == '.')) str
  in case tail of
    "oz" => Just (Ounce,     cast decimal)
    "g"  => Just (Gram,      cast decimal)
    "lb" => Just (Pound,     cast decimal)
    "kg" => Just (KiloGram,  cast decimal)
    "mg" => Just (MilliGram, cast decimal)
    _    => Nothing


||| Parse a line of text into a command
partial
parseCommand : String -> Maybe Command
parseCommand ""  = Just Quit
parseCommand str = case strIndex str 0 of
  '*' => map Scan  $ fromDigits  $ strTail str
  '/' => map NetWt $ parseNetWeight $ strTail str
  '$' => Just $ Name $ strTail str
  _   => Nothing



{- Entry Point ---------------------------------------------------------------}


covering export
run : Channel Command -> Inventory -> IO ()
run chan inv = do
  putStrLn $ show inv
  cmd <- channelGet chan
  putStrLn $ show cmd
  case !(eval inv cmd) of
    Nothing => pure ()
    Just inv => run chan inv

||| Read and decode command strings from stdin
partial
readStdin : Channel Command -> IO ()
readStdin chan = do
  line <- getLine
  putStrLn $ show line
  case parseCommand line of
    Nothing => do
      putStrLn $ "Invalid Command: " ++ line
      readStdin chan
    Just cmd => do
      channelPut chan cmd
      readStdin chan

||| Inject scale events into the command queue as appropriate
inject_weight : Channel Command -> USBScale.Result -> IO ()
inject_weight _ Empty = pure ()
inject_weight _ Weighing = pure ()
inject_weight _ (Fault str) = pure ()
inject_weight c (Ok w) = channelPut c $ Weigh w

||| Entry point for the `inventory` subcommand.
partial export
main : List String -> IO ()
main [inv_path, scale_path] = do
  chan      <- makeChannel
  Right inv <- openInventory inv_path | Left err => putStrLn err
  _         <- USBScale.spawn scale_path (inject_weight chan)
  _         <- fork (run chan inv)
  readStdin chan
{- main [inv_path] = do
  chan      <- makeChannel
  Right inv <- openInventory inv_path | Left err => putStrLn err
  _         <- fork (run chan inv)
  readStdin chan -}
main _ = do
  putStrLn "No path given"
