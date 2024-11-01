{-
 - A Meal Planner in Idris
 - Copyright (C) 2022 Brandon Lewis
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU Affero General Public License as
 - published by the Free Software Foundation, either version 3 of the
 - License, or (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU Affero General Public License for more details.
 -
 - You should have received a copy of the GNU Affero General Public License
 - along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}


||| A Food Database
module Food


import Measures
import Barcode
import Data.SortedMap
import Data.SortedSet
import JSON.Derive
import JSON
import System

import TUI
import TUI.MainLoop.Default
import TUI.Component
import TUI.Component.Form
import TUI.Component.Table
import TUI.Component.FocusRing


%default total
%language ElabReflection


||| A spinner widget which contains all the units valid for `d`
export
unitSelector : {d : Dimension} -> UnitT d -> Component (UnitT d)
unitSelector {d} u = Spinner.fromChoice @{show} {
  choices = Units,
  choice = u,
  has = unitInUnits
}

||| A numeric input and a quantity selector.
quantityC : {d : Dimension} -> Quantity d -> Component (Quantity d)
quantityC q = mapMaybe validate $ FocusRing.horizontal {
  items = [numeric q.amount, unitSelector q.unit],
  selection = 0,
  onKey = onKey
} where
  validate : All Maybe [Double, UnitT d] -> Maybe (Quantity d)
  validate [Just amount, Just unit] = Just $ Q amount unit
  validate _                        = Nothing

  onKey : Component.Handler (FocusRing [Double, UnitT d]) (All Maybe [Double, UnitT d]) Key
  onKey Left   self = update $ prev self
  onKey Right  self = update $ next self
  onKey Enter  self = yield $ self.values
  onKey Escape self = exit
  onKey k      self = handleSelected k self

||| Nutritional data
public export
record NutritionB f where
  constructor N
  servingSize : f Weight
  values : f (SortedMap String Weight)
%runElab derive "NutritionB" [Show, Eq, ToJSON, FromJSON]

||| Nutrient input
||| A numeric input and a quantity selector.
nutrientsC
  : SortedMap String Weight
  -> Component (List $ All Maybe [String, Double, UnitT Mass])
nutrientsC values = table {
  labels = ["Nutrient", "Amount", "Unit"],
  rows = mkRow <$> toList values,
  onKey = onKey
} where
  Tys : Vect 3 Type
  Tys = [String, Double, UnitT Mass]

  0 RowT : Type
  RowT = All Component Tys

  0 ValueT : Type
  ValueT = All Maybe Tys

  mkRow : (String, Weight) -> RowT
  mkRow (nutrient, Q amount unit) = [
    textInput nutrient,
    numeric amount,
    unitSelector unit
  ]

  onKey : Component.Handler (Table Tys) (List ValueT) Key
  onKey Left        self = update $ goLeft self
  onKey Right       self = update $ goRight self
  onKey Up          self = update $ goUp self
  onKey Down        self = update $ goDown self
  onKey Tab         self = update $ next self
  onKey Escape      self = exit
  onKey (Alpha '+') self = update $ insert (mkRow ("New", 100.g)) self
  onKey (Alpha '[') self = handleSelected Up self
  onKey (Alpha ']') self = handleSelected Down self
  onKey k           self = handleSelected k self

0 Nutrition : Type
Nutrition = NutritionB id

{-
nutritionForm : Nutrition -> Component Nutrition
nutritionForm n = mapMaybe validate $ FocusRing.vertical {
  items = [quantityC n.servingSize, vlist header toList onListKey],
  selection = 0,
  onKey = onTopLevelKey
} where
  onListKey : Component.Handler (VList 
--nutritionForm = ariaForm 
-}

||| Abstract food type
public export
record FoodB f where
  constructor MkFood
  name:         f String
  brand:        f $ String
  barcode:      f $ Barcode
  nutrition:    f $ Nutrition
%runElab derive "FoodB" [Show, Eq, ToJSON, FromJSON]

||| A complete, valid food record that can be stored in a database.
public export 0 Food : Type ; Food = FoodB id

{-
foodForm : Food -> Component Food
foodForm food = 
-}

export
View (Food) where
  size = ?hsize
  paint state window self = ?hpaint

{-
||| Construct a food from a food editor.
validate : FoodEditor -> Maybe Food
validate self = do
  Just $ MkFood
    !self.name
    !self.brand
    !self.barcode
    !self.nutrition

editor : Food -> FoodEditor
editor self = MkFood
  (Just self.name)
  (Just self.brand)
  (Just self.barcode)
  (Just self.nutrition)
-}

testNutrients : SortedMap String Weight
testNutrients = fromList $ [
  ("Fat", 2.g),
  ("Sodium", 380.mg),
  ("Dietary Fiber", 5.g),
  ("Total Sugars", 1.g),
  ("Protein", 4.g)
]

export
main : List String -> IO ()
main [] = do
  putStrLn $ show $ !(runComponent !getDefault (nutrientsC testNutrients))
main _ = die "Invalid subcommand"
