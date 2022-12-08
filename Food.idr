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


module Food


-- Map food name to preferred source
export total
source : String -> String
source n@"Bread Slice"       = "Grocery Outlet"
source n@"Pizza Sauce"       = "Scratch"
source n@"Mozarella"         = "Grocery Outlet"
source n@"Italian Sausage"   = "Longs"
source n@"Tortilla"          = "Grocery Outlet"
source n@"Red Pepper Flakes" = "Winco"
source n@"Ginger Root"       = "Sunrise"
source n@"White Pepper"      = "Kiva"
source _                     = "Any"

