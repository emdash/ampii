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

module Main


import System
import Inventory
import USBScale

partial
dispatch : String -> List String -> IO ()
dispatch "inventory" rest = Inventory.main rest
dispatch "scale"     rest = USBScale.main rest
dispatch sc          _    = putStrLn ("Invalid subcommand: " ++ sc)

partial
main : IO ()
main = do
  args  <- getArgs
  case args of
    []              => putStrLn "Impossible"
    [_]             => putStrLn "No subcommand"
    _ :: sc :: rest => dispatch sc rest
