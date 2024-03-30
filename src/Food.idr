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

%default total
%language ElabReflection


ToJSON a => ToJSON (SortedSet a) where
  toJSON = toJSON . SortedSet.toList

ToJSON a => ToJSON (SortedMap String a) where
  toJSON x = object $ map (mapSnd toJSON) $ SortedMap.toList x

Ord a => FromJSON a => FromJSON (SortedSet a) where
  fromJSON x = SortedSet.fromList <$> fromJSON x

Ord a => FromJSON a => FromJSON (SortedMap String a) where
  fromJSON x = SortedMap.fromList <$> fromJSON x


||| Nutritional data
public export
record Nutrition where
  servingSize : Weight
  values : SortedMap String Weight
%runElab derive "Nutrition" [Show, Eq, ToJSON, FromJSON]


||| Data regarding a particular kind of food.
public export
record Food where
  name:         String
  brand:        Maybe String
  barcode:      Barcode
  nutrition:    Nutrition
  imagePath:    String
  sources:      SortedSet String

%runElab derive "Food" [Show, Eq, ToJSON, FromJSON]
