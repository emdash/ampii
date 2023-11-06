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
import TUI


%default total
%language ElabReflection
%hide Measures.Unit

||| Nutritional data
public export
record Nutrition where
  constructor N
  servingSize : Weight
  values : SortedMap String Weight
%runElab derive "Nutrition" [Show, Eq, ToJSON, FromJSON]

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
||| A food variant where every field is optional, allowing us to
||| construct it incrementally.
export 0 FoodEditor : Type ; FoodEditor = AFood Maybe

export
View (FoodEditor) where
  size = ?hsize
  paint state window self = ?hpaint

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
