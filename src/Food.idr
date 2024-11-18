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
import TUI.MainLoop.InputShim
import TUI.Component
import TUI.Component.Form
import TUI.Component.Table
import TUI.Component.FocusRing
import TUI.Util

import Data.Vect.Quantifiers as VQ

%default total
%language ElabReflection


||| Nutritional data
public export
Nutrition : Type
Nutrition = SortedMap String (Quantity Mass)

||| Single Nutrient Datum
public export
Nutrient : Type
Nutrient = (String, Quantity Mass)

||| Construct a nutrient from its values
public export
nutrient : String -> Double -> UnitT Mass -> Nutrient
nutrient name amount unit = (name, Q amount unit)

||| Abstract food type
public export
record Food where
  constructor MkFood
  name        : String
  brand       : String
  barcode     : Barcode
  servingSize : Quantity Mass
  calories    : Double
  nutrition   : Nutrition
%runElab derive "Food" [Show, Eq, ToJSON, FromJSON]

||| A spinner widget which contains all the units valid for `d`
export
unitSelector
  :  {0 events : List Type}
  -> Has Key events
  => {d : Dimension}
  -> UnitT d
  -> Component (HSum events) (UnitT d)
unitSelector {d} u = Spinner.fromChoice @{%search} @{show} {
  choices = Units,
  choice = u,
  has = unitInUnits
}

||| A numeric input and a quantity selector.
quantityC
  :  {0 events : List Type}
  -> Has Key events
  => {d : Dimension}
  -> Quantity d
  -> Component (HSum events) (Quantity d)
quantityC q = mapMaybe validate $ FocusRing.horizontal {
  items = [numeric q.amount, unitSelector q.unit],
  selection = 0,
  onKey = only onKey
} where
  validate : VQ.All.All Maybe [Double, UnitT d] -> Maybe (Quantity d)
  validate [Just amount, Just unit] = Just $ Q amount unit
  validate _                        = Nothing

  onKey : Single.Handler
    {events}
    (FocusRing {events} [Double, UnitT d])
    (VQ.All.All Maybe [Double, UnitT d]) Key
  onKey Left   self = update $ prev self
  onKey Right  self = update $ next self
  onKey Enter  self = yield $ self.values
  onKey Escape self = exit
  onKey k      self = handleSelected (inject k) self

||| Nutrient input
nutritionC
  :  {0 events : List Type}
  -> Has Key events
  => SortedMap String Weight
  -> Component (HSum events) (SortedMap String Weight)
nutritionC values = mapMaybe validate $ table {
  labels = ["Nutrient", "Amount", "Unit"],
  rows = mkRow <$> toList values,
  handler = only onKey
} where
  ||| The type of each column in the table
  Tys : Vect 3 Type
  Tys = [String, Double, UnitT Mass]

  ||| Try to convert one row of the table into a nutrient value
  validateRow : All Maybe Tys -> Maybe Nutrient
  validateRow values = happly nutrient <$> allIsJust values

  ||| Try to validate the entire table.
  validate : List (All Maybe Tys) -> Maybe (SortedMap String Weight)
  validate rows = insertFrom' empty <$> traverse validateRow rows

  ||| Construct a new table row.
  mkRow : (String, Weight) -> All (Component (HSum events)) Tys
  mkRow (nutrient, Q amount unit) = [
    textInput nutrient,
    numeric amount,
    unitSelector unit
  ]

  ||| Do some ad-hoc key handling for now.
  |||
  ||| Composition of components is still not quite right.
  onKey : Single.Handler {events} (Table {events} Tys) (List (All Maybe Tys)) Key
  onKey Left        self = update $ goLeft self
  onKey Right       self = update $ goRight self
  onKey Up          self = update $ goUp self
  onKey Down        self = update $ goDown self
  onKey Tab         self = update $ next self
  onKey Escape      self = exit
  onKey (Alpha '+') self = update $ insert (mkRow ("New", 100.g)) self
  onKey (Alpha '[') self = handleSelected (inject Key.Up) self
  onKey (Alpha ']') self = handleSelected (inject Key.Down) self
  onKey k           self = handleSelected (inject k) self

foodC
  :  Food
  -> Component (HSum [List Bits8, String, Key]) Food
foodC food = happly MkFood <$> ariaForm [
  F "Name"         $ textInput food.name,
  F "Brand"        $ textInput food.brand,
  F "Barcode"      $ mapMaybe fromDigits $ textInput $ toString food.barcode,
  F "Serving Size" $ quantityC food.servingSize,
  F "Calories"     $ numeric food.calories,
  F "Nutrition"    $ nutritionC food.nutrition
]

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
  putStrLn $ show $ !(runComponent !mainLoop (foodC missionWholeWheatOriginal))
where
    mainLoop : IO (InputShim [List Bits8, String, Key])
    mainLoop = do
      mainLoop <- inputShim
      image <- raw {eventT = String}     "Image"
      scale <- raw {eventT = List Bits8} "Scale"
      pure $ (mainLoop.addEvent image).addEvent scale

main _ = die "Invalid subcommand"
