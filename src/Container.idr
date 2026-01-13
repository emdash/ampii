module Container

import Barcode
import Date
import DirDB
import Food
import JSON.Derive
import Measures
import System
import System.File
import System.File.ReadWrite
import System.File.Virtual
import TUI


%default total
%language ElabReflection



||| The type of container IDs.
|||
||| For now these are just arbitrary string keys.
public export
0 Id : Type
Id = Vect 4 Char

||| Implement PathSafe for container IDs
|||
||| Container ids are vectors of four characters. They are safe to
||| encode as paths, since each character must be a digit. Ordinary
||| barcodes cannot encode non-numeric characters, so anything other
||| than digits is unexpected.
export
PathSafe Id where
  toPath self = toMaybe (all isDigit self) $ delay $ pack $ toList self
  fromPath self = toVect 4 $ unpack self

||| How long a given food item is expected to last.
|||
||| This is meant to capture the different representations of product
||| life which are used on packaged and unpackaged foods. From this we
||| can calculate when action must be taken for a given food, and
||| prompt the user to take appropriate actions.
public export
data LifeTime
  = BestBy        Date
  | UseBy         Date
  | UseOrFreezeBy Date
  | Expires       Date
  | Forever
  | Unknown
%runElab derive "LifeTime" [Show,Eq,FromJSON,ToJSON]

||| Conservatively report the expiry date of a container
public export
expiryDate : LifeTime -> Maybe Date
expiryDate (BestBy x)        = Just $ x
expiryDate (UseBy x)         = Just $ x
expiryDate (UseOrFreezeBy x) = Just $ x
expiryDate (Expires x)       = Just $ x
expiryDate Forever           = Nothing
expiryDate Unknown           = Nothing

||| The type of container.
public export
data ContainerType
  = WholeFood
  | Sealed
  | Opened
  | Reusable
%runElab derive "ContainerType" [Show,Eq,FromJSON,ToJSON]

||| A container holds an amount of some food.
|||
||| Each container has a unique ID assigned by the user.
|||
||| For each container, we track its type, lifetime, gross weight, and
||| tear weight.
|||
||| This is the generic container, parameterized over the food
||| representation.
public export
record ContainerB foodT where
  constructor MkContainer
  id    : Id
  food  : Maybe foodT
  life  : LifeTime
  type  : ContainerType
  tear  : Weight
  gross : Weight
%runElab derive "ContainerB" [Show,Eq,FromJSON,ToJSON]

||| Project food barcode out of a container.
|||
||| This is an interface so we can implement it for different concrete
||| containers.
export
interface HasBarcode foodT where
  (.barcode) : ContainerB foodT -> Maybe Barcode

||| Container records in a database store the barcode for the food,
||| instead of the food record.
namespace Raw
  public export
  0 Container : Type
  Container = ContainerB Barcode

  export HasBarcode (Barcode) where (.barcode) = (.food)

||| Containers returned from queries joined on Food.
namespace Joined
  ||| Container with Food joined onto it.
  public export
  0 Container : Type
  Container = ContainerB Food

  export HasBarcode (Food) where self.barcode = (.barcode) <$> self.food

||| Make a new container with the given id.
export
empty : Id -> ContainerType -> ContainerB _
empty id type = MkContainer {
  id    = id,
  food  = Nothing,
  life  = Unknown,
  type  = type,
  tear  = 0.g,
  gross = 0.g
}

||| Project the net weight from Container.
export
(.net) : ContainerB _ -> Weight
(.net) self = self.gross - self.tear

||| Change the food assigned to the container.
export
assign : foodT -> ContainerB foodT -> ContainerB foodT
assign food = { food := Just food }

||| True if the container expires on or after the given date.
public export
expiresOn : Date -> ContainerB _ -> Bool
expiresOn date self = case expiryDate self.life of
  Nothing => False
  Just e  => date <= e

||| Updates tear weight on the container.
export
setTear : Weight -> ContainerB foodT -> ContainerB foodT
setTear w = { tear := w }

||| Updates the gross weight on the container.
export
setGross : Weight -> ContainerB foodT -> ContainerB foodT
setGross w = { gross := w }

||| Update the tear weight by subtracting given net weight from the
||| current gross weight.
|||
||| e.g. if we had `{ tear = 30.g, gross = 345.g }`, and gave a net of
||| `320.g`, we'd get `{ tear = 25, gross = 345.g }`. This is used
||| when adding new containers to the inventory, to allow the user to
||| subtract the printed net weight from the scale weight.
export
setTearFromNet : Weight -> ContainerB foodT -> ContainerB foodT
setTearFromNet net self = { tear := self.gross - net } self

||| Reset both weights to zero.
export
reset : ContainerB foodT -> ContainerB foodT
reset = { tear := 0.g, gross := 0.g }

||| True if the container has the given barcode.
|||
||| This will match either the container's unique ID, or the UPC
||| for the associated food.
export
hasBarcode : HasBarcode foodT => Barcode -> ContainerB foodT -> Bool
hasBarcode (User id) self = id      == self.id
hasBarcode barcode   self = fromMaybe False $ (barcode ==) <$> self.barcode

export
View Id where
  size _ = MkArea 4 1
  paint state window self = paint @{show} state window (User self)

||| This is the 'row' View for container.
export
View (ContainerB _) where
  -- size here is just a guess, but it should be a fixed grid
  -- up to 13 chars for the barcode, plus padding
  -- 10 digits each for gross, tear, and net
  size _ = MkArea (14 + 10 + 10 + 10) 1

  paint state window self = do
    let (top,     bottom) = window.splitTop 1
    let (barcode, top   ) = top.splitLeft 13
    let (tear,    top   ) = top.splitLeft 10
    let (gross,   net   ) = top.splitLeft 10
    paint         state barcode self.id
    paint @{show} state tear    self.tear
    paint @{show} state gross   self.gross
    paint @{show} state net     self.net

||| Open a todo list file, and try to parse its contents into a list
||| of items. This doesn't do much in the way of error handling.
export covering
fromFile : String -> IO (Maybe (List $ Raw.Container))
fromFile path = do
  putStrLn "read file \{path}"
  Right contents <- readFile path | Left err => pure Nothing
  putStrLn "contents"
  case decode contents of
    Left  err      => pure Nothing
    Right contents => pure $ Just contents

||| Save the given list of items to the given path as a JSON document.
export covering
toFile : String -> List Raw.Container -> IO ()
toFile path todolist = do
  ignore $ writeFile path $ encode todolist
