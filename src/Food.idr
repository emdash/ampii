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

import Data.Vect.Quantifiers as VQ

import JSON.Simple
import JSON.Simple.Derive

import TUI
import TUI.MainLoop.Async
import TUI.Component
import TUI.Component.Form
import TUI.Component.Table
import TUI.Component.FocusRing
import TUI.Util

import Measures
import Barcode
import Data.SortedMap
import Data.SortedSet
import System


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

||| Get the calorig content as a ratio of Calories per 100g
|||
||| XXX: handle this in measures properly
export
(.calsPer100g) : Food -> Double
(.calsPer100g) self = self.calories * 100 / (cast (self.servingSize `as` Grams))

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

fields : List String
fields = [
  "Name",
  "Brand",
  "Barcode",
  "Serving Size",
  "Calories"
]

export
View Food where
  size = const $ sizeVertical fields
  paint state window self = do
    let split = sizeVertical fields
    let (left, right) = window.splitRight (cast split.width)
    right <- packLeft state right VRule

    left  <- packTop state left  "Name"
    right <- packTop state right self.name

    left  <- packTop state left  "Brand"
    right <- packTop state right self.brand

    left  <- packTop state left  "Barcode"
    right <- packTop state right self.barcode

    left  <- packTop state left  "Serving Size"
    right <- packTop state right $ show self.servingSize

    left  <- packTop state left  "Calories/100g"
    ignore $ packTop state right $ show self.calsPer100g

foodC
  :  {0 events : List Type}
  -> Has Key events
  => Food
  -> Component (HSum events) Food
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
main args = do
  putStrLn $ show $ !(runComponent mainLoop ui)
where
    ui : Component (HSum [Key]) Food
    ui = foodC missionWholeWheatOriginal

    mainLoop : AsyncMain [Key]
    mainLoop = asyncMain []
