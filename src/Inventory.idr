||| Tracks the user's groceries.
|||
||| The inventory is stored as directory of JSON-encoded files, one
||| for each pantry item.
|||
||| The inventory tracks food at the granularity of individual
||| containers of food.
module Inventory


import Data.List1
import Data.Vect
import Data.SortedMap
import JSON.Derive
import Data.String
import System
import System.File
import System.Directory
import System.Concurrency

import Container
import Barcode
import Control.ANSI
import Date
import DirDB
import Food
import Measures
import USBScale

%default total
%language ElabReflection


{- Search --------------------------------------------------------------------}

||| The different ways we can query the inventory.
data Query
  = ByBarcode     Barcode
  | ExpiresOn     Date
  | ByFoodName    String
  | And           Query Query
  | All
%runElab derive "Query" [Show, Eq]

||| An Inventory is a set of containers indexed by their Id
0 Inventory : Type
Inventory = SortedMap Id Container

query : Query -> Id -> Container -> Bool
query (ByBarcode x)    _ c = c.food == x
query (ExpiresOn x)    _ c = expiresOn x c.life
query (ByFoodName str) _ c = ?hole
query (And a b)        i c = (query a i c) && (query b i c)
query All              _ _ = True


{- Command line parsing ****************************************** -}

||| How to get user input for a value.
|||
||| Not all modes of input make sense for all types, so this is
||| indexed over the type of the data, with specific constructors for
||| each type.
data Prompt : a -> Type where
  Direct     : a     -> Prompt a
  FromScale  :          Prompt Weight
  QueryFood  : Query -> Prompt Barcode
  ChooseFood :          Prompt Barcode
  QueryId    : Query -> Prompt Id
  ChooseId   :          Prompt Id

Show a => Show (Prompt a) where
  show (Direct x)    = "(Direct \{show x})"
  show FromScale     = "FromScale"
  show (QueryFood x) = "(QueryFood \{show x})"
  show ChooseFood    = "ChooseFood"
  show (QueryId x)   = "(QueryId \{show x})"
  show ChooseId      = "ChooseId"


||| The high level commands on the inventory.
|||
||| Where the user has a choice of input methods, we wrap the required
||| type in "Prompt".
data Command
  = Search   Query
  | Show     (Prompt Id)
  | Weigh    (Prompt Id) (Prompt Weight)
  | Create   (Prompt Id)
             (Prompt Barcode)
             LifeTime
             ContainerType
             (Prompt Weight)
             (Prompt Weight)
  | Transfer (Prompt Id) (Prompt Id)
  | Delete   (Prompt Id)
%runElab derive "Command" [Show]


||| Parse a single term in a query.
|||
||| A term is either a bare word, or prefixed with an operator, as is
||| done in email clients and issue trackers.
parseQueryTerm : String -> Maybe Query
parseQueryTerm term = case toList $ split (== ':') term of
  ["all"]        => Just All
  [term]         => Just $ ByFoodName term
  ["name",    n] => Just $ ByFoodName n
  ["barcode", b] => map ByBarcode  $ fromDigits b
  ["expires", d] => map ExpiresOn  $ fromString d
  _              => Nothing

||| Recursively parse an entire query
|||
||| Juxtaposed terms are implicitly combined via `And`
parseQuery : List String -> Maybe Query
parseQuery [] = Nothing
parseQuery [term] = parseQueryTerm term
parseQuery (term  :: rest)  = Just $ And !(parseQueryTerm term) !(parseQuery rest)

||| XXX: this can't handle the number 0, so is broken.
parseNat : String -> Maybe Nat
parseNat s = case stringToNatOrZ s of
  Z => Nothing
  x => Just x

||| Parse an ID in a command
|||
||| The special value `choose` will prompt the user to choose from a list.
parseId : String -> Maybe $ Prompt Id
parseId "choose" = Just ChooseId
parseId id       = Just $ Direct id

||| Parse a barcode in a command
|||
||| The special value `choose` will prompt the user to choose from a list.
parseBarcode  : String      -> Maybe $ Prompt Barcode
parseBarcode "choose" = Just ChooseFood
parseBarcode bc = map Direct $ fromDigits bc

||| Parse a container lifetime
|||
||| This is either one of the special keywords `forever`, `unknown`,
||| or a prefixed date or day value.
parseLifeTime : String -> Maybe LifeTime
parseLifeTime "forever" = Just Forever
parseLifeTime "unknown" = Just Unknown
parseLifeTime lifetime =
  let
    parts := toList $ split (== ':') lifetime
  in case parts of
    ["best-by", date]          => map BestBy        $ fromString date
    ["use-by",  date]          => map UseBy         $ fromString date
    ["use-or-freeze-by", date] => map UseOrFreezeBy $ fromString date
    ["expires", date]          => map Expires       $ fromString date
    _                          => Nothing

||| Parse container type in a command line
parseContainerType: String  -> Maybe ContainerType
parseContainerType "whole"    = Just WholeFood
parseContainerType "opened"   = Just Opened
parseContainerType "sealed"   = Just Sealed
parseContainerType "resuable" = Just Reusable
parseContainerType _          = Nothing

||| Parse a mass value in a command line.
parseWeight : String -> Maybe $ Prompt Weight
parseWeight "scale" = Just $ FromScale
parseWeight str = map Direct $ quantityFromString str

||| Collect the complete inventory record from the user.
parseCreateCmd : List String -> Maybe Command
parseCreateCmd [id, bc, lt, ct, ew, cw] = do
  id <- parseId id
  bc <- parseBarcode bc
  lt <- parseLifeTime lt
  ct <- parseContainerType ct
  ew <- parseWeight ew
  cw <- parseWeight cw
  pure $ Create id bc lt ct ew cw
parseCreateCmd _ = Nothing

||| Parse the entire command line
parse : List String -> Maybe Command
parse []                     = Nothing
parse ("search" :: query)    = map Search $ parseQuery query
parse ["show",   id]         = map Show   $ parseId id
parse ["weigh",  id, weight] = Just $ Weigh !(parseId id) !(parseWeight weight)
parse ("create" :: rest)     = parseCreateCmd rest
parse ["transfer", a, b]     = Just $ Transfer !(parseId a) !(parseId b)
parse ["delete", id]         = map Delete $ parseId id
parse _                      = Nothing


{- Configuration -------------------------------------------------------------}

record Config where
  constructor MkConfig
  scale: String
  db:    Handle Id Container

covering
getConfig : IO Config
getConfig = do
  Just scale <- getEnv "AMPII_SCALE_PATH" | Nothing => die "No scale path"
  Just db    <- getEnv "AMPII_DB_PATH"    | Nothing => die "No database path"
  Right h    <- connect db                | Left e  => die e
  pure $ MkConfig scale h


PathSafe String where
  toPath = id
  fromPath = id

Serializable Container where

Serializable Id where

Row Id Container where

{- Command Processing --------------------------------------------------------}

0 Result : Type -> Type
Result a = IO (Either Error a)

ok : Inventory.Result Builtin.Unit
ok = pure $ Right ()

fail : Error -> Inventory.Result a
fail e = pure $ Left e


covering
runPrompt : Config -> Prompt x -> IO x
runPrompt cfg (Direct y) = pure y
runPrompt cfg FromScale = case !(getWeight cfg.scale) of
  Left  err => die "couldn't read scale"
  Right weight => pure weight
runPrompt cfg (QueryFood y) = ?runPrompt_rhs_2
runPrompt cfg ChooseFood = ?runPrompt_rhs_3
runPrompt cfg (QueryId y) = ?runPrompt_rhs_4
runPrompt cfg ChooseId = ?runPrompt_rhs_5

covering
run : Config -> Command -> Inventory.Result Builtin.Unit
run cfg (Search q)  = do
  Right result <- select (query q) cfg.db | Left e => fail e
  printRows result
  ok
run cfg (Show x)    = do
  id <- runPrompt cfg x
  Right cont <- readRow cfg.db id | Left e => fail e
  putStrLn $ show cont
  ok
run cfg (Weigh id w) = do
  id <- runPrompt cfg id
  Right cont <- readRow cfg.db id | Left e => fail e
  cw <- runPrompt cfg w
  Right _ <- writeRow cfg.db id $ {current := cw} cont | Left e => fail e
  ok
run cfg (Create id bc lt ct ew cw) = do
  id <- runPrompt cfg id
  bc <- runPrompt cfg bc
  ew <- runPrompt cfg ew
  cw <- runPrompt cfg cw
  Right _ <- writeRow cfg.db id $ MkContainer bc lt ct ew cw
          | Left e => fail e
  ok
run _ (Transfer x y) = ?hole_4
run cfg (Delete id) = do
  id <- runPrompt cfg id
  Right _ <- deleteRow cfg.db id | Left e => fail e
  putStrLn "Deleted: \{id}"
  ok


{- Entry Point ---------------------------------------------------------------}


||| Entry point for the `inventory` subcommand.
partial export
main : List String -> IO Builtin.Unit
main args = do
  config <- getConfig
  case parse args of
    Nothing => putStrLn "Invalid command: \{unwords args}"
    Just cmd => do
      putStrLn $ show cmd
      Right _ <- run config cmd | Left err => die err
      pure ()
