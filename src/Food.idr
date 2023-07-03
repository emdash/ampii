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


%default total


||| Maps a food name to a preferred source for said food.
|||
||| XXX: I haven't thought too deeply about what constitutes
||| `source`. For now it's just taken to be a unique string.
|||
||| XXX: Return type should be `Set String`. A food could have
||| multiple preferred sources, possibly with a (partial or
||| total)-ordering across sources (either per-food or globally). The
||| reason I'm not modeling this right now is that I'm not sure of the
||| best way to present that information. For now grouping things by a
||| single store is good enough.
export
source : String -> String
source n@"Bread Slice"        = "Grocery Outlet"
source n@"Pizza Sauce"        = "Scratch"
source n@"Mozarella"          = "Grocery Outlet"
source n@"Italian Sausage"    = "Longs"
source n@"Tortilla"           = "Grocery Outlet"
source n@"Red Pepper Flakes"  = "Winco"
source n@"Ginger Root"        = "Sunrise"
source n@"White Pepper"       = "Kiva"
source n@"Sichuan Peppercorn" = "Sunrise"
source n@"Doubanjiang"        = "Sunrise"
source _                      = "Any"


||| Map food name to a density in g/mL, where appropriate
density : String -> Maybe Double
density _ = Nothing

||| XXX: kindof duplicating work, but will need to refactor in order
||| to allow using units here instead of double
ounce : Double -> Double
ounce x = gramsPerOz * x

||| Map food Size, Food to weight in grams
|||
||| XXX: interval math?
weights : String -> Size -> Maybe Double
-- onions
-- source: https://howdykitchen.com/medium-onion-size/
weights "Onion" Small  = Just (ounce 4.0)
weights "Onion" Med    = Just (ounce 7.0)
weights "Onion" Large  = Just (ounce 11.0)
-- egg sizes in the US are specified per dozen, not per egg, hence the
-- divisor.
-- source: https://www.peteandgerrys.com/blog/egg-size-guide
weights "Egg"   Small  = Just (ounce 18.0 / 12.0)
weights "Egg"   Med    = Just (ounce 21.0 / 12.0)
weights "Egg"   Large  = Just (ounce 24.0 / 12.0)
weights "Egg"   XLarge = Just (ounce 27.0 / 12.0)
weights "Egg"   Jumbo  = Just (ounce 30.0 / 12.0)
-- error case
weights _      _       = Nothing


export 
Material String where
  density = Food.density
  weights = Food.weights
