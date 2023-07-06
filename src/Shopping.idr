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


||| Generates a shopping list from a list of recipes.
module Shopping

import Data.SortedMap
import Recipes

-- XXX: fix issues with Quantity signature
-- import Measures
import Food


%default total


-- XXX: dummy definition
data Quantity : Type where



||| A helper function for foldIngredients
foldQuantity
  :  SortedMap String Quantity
  -> Ingredient
  -> SortedMap String Quantity
{-
foldQuantity accum (I food amount) = case lookup food accum of
  Nothing => insert food amount                 accum
  Just a  => insert food (addQuantity a amount) accum
-}

||| Fold a list of ingredients into mapping from Name -> Quantity
foldIngredients : List Ingredient -> SortedMap String Quantity -> SortedMap String Quantity
{-
foldIngredients []        ret = ret
foldIngredients (x :: xs) ret = (foldQuantity (foldIngredients xs ret) x)
-}

||| Fold a list of recipes into a mapping from Name -> Quantity
foldRecipes : List String -> SortedMap String Quantity
{-
foldRecipes        [] = SortedMap.empty
foldRecipes (r :: rs) = foldIngredients (ingredients (recipes r)) (foldRecipes rs)
-}

||| Recursive helper for groupBySource
groupBySourceRec
  :  List (String, Quantity)
  -> SortedMap String (List (String, Quantity))
  -> SortedMap String (List (String, Quantity))
{-
groupBySourceRec []        accum = accum
groupBySourceRec (x :: xs) accum = 
  let
    (name, _) = x
    src  = source name
    rest = groupBySourceRec xs accum
  in case (SortedMap.lookup src rest) of
    Nothing => insert src [x]      rest
    Just s  => insert src (x :: s) rest
-}

||| Group ingredients by preferred source
export
groupBySource : SortedMap String Quantity -> List (String, (List (String, Quantity)))
groupBySource rs = toList (groupBySourceRec (toList rs) SortedMap.empty)

||| Return a shopping list for the given list of recipes
export
shoppingList : List String -> List (String, List (String, Quantity))
shoppingList rs = groupBySource (foldRecipes rs)
