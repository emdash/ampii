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


||| Food-Specific Conversion Factors
module Food


import Measures


||| Maps a food name to a preferred source for said food.
|||
||| XXX: I haven't thought too deeply about what constitutes
||| `source`. It's a retail store 99% of the time, but could be a
||| neighbor, a buyer's club, your garden, etc. For now it's just
||| taken to be a unique string.
|||
||| XXX: These should be `Data.AVL.Set String`. A food could have
||| multiple preferred sources, possibly with a (partial or
||| total)-ordering across sources (either per-food or globally).
export total
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
total
density : String -> Maybe Double
density _ = Nothing

||| Map food Size, Food to weight in grams
-- tbd: this is where interval math would be injected.
total
avgWeight : String -> Size -> Maybe Double
-- onions
-- source: https://howdykitchen.com/medium-onion-size/
avgWeight "Onion" Small  = Just (Oz 4.0)
avgWeight "Onion" Medium = Just (Oz 7.0)
avgWeight "Onion" Large  = Just (Oz 11.0)
-- egg sizes in the US are specified per dozen, not per egg, hence the
-- divisor.
-- source: https://www.peteandgerrys.com/blog/egg-size-guide
avgWeight "Egg"   Small  = Just (Oz 18.0 / 12.0)
avgWeight "Egg"   Medium = Just (Oz 21.0 / 12.0)
avgWeight "Egg"   Large  = Just (Oz 24.0 / 12.0)
avgWeight "Egg"   XLarge = Just (Oz 27.0 / 12.0)
avgWeight "Egg"   Jumbo  = Jumbo(Oz 30.0 / 12.0)
-- error case
avgWeight _      _       = Nothing

