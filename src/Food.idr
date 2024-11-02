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


||| Nutritional data
public export
0 Nutrition : Type
Nutrition = SortedMap String Weight

||| Abstract food type
public export
record Food where
  constructor MkFood
  name        : String
  brand       : String
  barcode     : Barcode
  servingSize : Weight
  calories    : Double
  nutrition   : Nutrition
%runElab derive "Food" [Show, Eq, ToJSON, FromJSON]

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

||| Nutrient input
||| A numeric input and a quantity selector.
nutritionC
  :  SortedMap String Weight
  -> Component (SortedMap String Weight)
nutritionC values = validate <$> table {
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

  validate : List ValueT -> SortedMap String Weight
  validate rows = fromList $ mapRows rows
    where
      mapRows : List ValueT -> List (String, Weight)
      mapRows [] = []
      --- XXX: UGGGGGLLYYYYYYYYYY!!!!!
      mapRows ([Just name, Just amount, Just unit] :: xs) = (name, Q amount unit) :: mapRows xs
      mapRows (_ :: xs) = mapRows xs

  mkRow : (String, Weight) -> RowT
  mkRow (nutrient, Q amount unit) = [
    textInput nutrient,
    numeric amount,
    unitSelector unit
  ]

  ||| Do some ad-hoc key handling for now.
  |||
  ||| Composition of components is still not quite right.
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

-- Style note: this is starting to look like what I'd imagined. There
-- is some funky punctuation here and there, but the fact that this is
-- a *form* comes shining through all the line noise.
--
-- it almost even works, except that it hangs on submit.
foodC : Food -> Component Food
foodC food = validate <$> ariaForm [
  F "Name"         $ textInput food.name,
  F "Brand"        $ textInput food.brand,
  F "Barcode"      $ mapMaybe fromDigits $ textInput $ show food.barcode,
  F "Serving Size" $ quantityC food.servingSize,
  F "Calories"     $ numeric food.calories,
  F "Nutrition"    $ nutritionC food.nutrition
] where
  validate : HVect [
    String,
    String,
    Barcode,
    Quantity Mass,
    Double,
    SortedMap String (Quantity Mass)
  ] -> Food
  validate [
    name,
    brand,
    barcode,
    servingSize,
    calories,
    nutrition
  ] = MkFood {
    name = name,
    brand = brand,
    barcode = barcode,
    servingSize,
    calories,
    nutrition
  }

partial
missionWholeWheatOriginal : Food
missionWholeWheatOriginal = MkFood {
  name  = "Whole Wheat, Original, Super Soft",
  brand = "Mission Foods",
  barcode = fromString "073731071076",
  servingSize = 45.g,
  calories = 110,
  nutrition = fromList $ [
    ("Fat", 2.g),
    ("Sodium", 380.mg),
    ("Dietary Fiber", 5.g),
    ("Total Sugars", 1.g),
    ("Protein", 4.g)
  ]
}

export partial
main : List String -> IO ()
main [] = do
  putStrLn $ show $ !(runComponent !getDefault (foodC missionWholeWheatOriginal))
main _ = die "Invalid subcommand"
