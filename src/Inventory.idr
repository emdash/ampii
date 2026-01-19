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
import JSON.Simple.Derive
import Data.String
import System
import System.File
import System.Directory
import System.Concurrency

import Container
import Barcode
import Control.ANSI
import Date
import Food
import Measures

%default total
%language ElabReflection

||| Associate a set of foods with a set of physical containers.
|||
||| There might be multiple containers associated with a given food.
export
record Inventory where
  constructor MkInventory
  containers : SortedMap Container.Id Raw.Container
  foods      : SortedMap Barcode Food
%runElab derive "Inventory" [Show, Eq, FromJSON, ToJSON]

export
empty : Inventory
empty = MkInventory {
  containers = empty,
  foods = empty
}

||| Look up the given barcode in the food table.
export
(.getFood) : Inventory -> Barcode -> Maybe Food
(.getFood) self bc = lookup bc self.foods

||| Look up the given container ID in the container table.
export
(.getContainer) : Inventory -> Container.Id -> Maybe Joined.Container
(.getContainer) self id =
  let cont = lookup id self.containers
  in { food $= join . (self.getFood <$>)  } <$> cont

||| Look up this barcode in the databse.
|||
||| There may be multiple results if the barcode is a UPC for which the user has
||| registered multiple containers.
export
(.lookupBarcode) : Inventory -> Barcode -> Maybe $ Either Food Joined.Container
--  a user barcode could be either a container or a food.
(.lookupBarcode) self (User id) = case self.getContainer id of
  Nothing => case self.getFood (User id) of
    Nothing => Nothing
    Just food => Just $ Left food
  Just cont => Just $ Right cont
(.lookupBarcode) self bc = case self.getFood bc of
  Nothing => Nothing
  Just food => Just $ Left food

export
(.updateContainer)
  :  Inventory
  -> Container.Id
  -> (Raw.Container -> Raw.Container)
  -> Inventory
(.updateContainer) self id f = { containers $= SortedMap.update (f <$>) id } self

export
(.updateFood)
  :  Inventory
  -> Barcode
  -> (Food -> Food)
  -> Inventory
(.updateFood) self id f = { foods $= SortedMap.update (f <$>) id } self

||| List all barcodes known to the inventory
export
(.barcodes) : Inventory -> List Barcode
(.barcodes) self = (User <$> keys self.containers) ++ keys self.foods

export
(.size) : Inventory -> Nat
(.size) self = (length $ keys $ self.containers) + (length $ keys $ self.foods)
