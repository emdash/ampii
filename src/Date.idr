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
module Date


import Data.Fin
imoprt Data.Integer
import Data.Nat
import Derive.Prelude
import JSON.Derive
import System.Clock


%default total
%language ElabReflection

||| Returns true if the given year is a leap year.
export
isLeapYear : (n: Nat) -> Bool
isLeapYear year =
  let
    mod4   := (modNatNZ year 4   SIsNonZero) == 0
    mod100 := (modNatNZ year 100 SIsNonZero) == 0
    mod400 := (modNatNZ year 400 SIsNonZero) == 0
  in
    (mod4) && (not (mod100) || mod400)

||| Returns the number of days of the given zero-indexed month.
export
daysOfMonth : Nat -> Fin 12 -> Nat
daysOfMonth y m = case isLeapYear y of
  True  => leap m
  False => nonLeap m
  where
    nonLeap : Fin 12 -> Nat
    nonLeap 0 = 31
    nonLeap 1 = 28
    nonLeap 2 = 31
    nonLeap 3 = 30
    nonLeap 4 = 31
    nonLeap 5 = 30
    nonLeap 6 = 31
    nonLeap 7 = 31
    nonLeap 8 = 30
    nonLeap 9 = 31
    nonLeap 10 = 30
    nonLeap 11 = 31

    leap : Fin 12 -> Nat
    leap m = case m of
      1 => 29
      x => nonLeap x

||| A type that represents a valid Julian Date
|||
||| Month and year are 0-indexed, so they can be represented with
||| `Fin`.
export
record Date where
  constructor MkDate
  year:  Nat
  month: Fin 12
  day:   Fin $ daysOfMonth year month


||| Construct a date from user input.
|||
||| This expects 1-based inputs for month and day.
date : Nat -> Nat -> Nat -> Maybe Date
date _    Z         _       = Nothing
date _    _         Z       = Nothing
date year (S month) (S day) = do
  m <- natToFin month 12
  d <- natToFin day $ daysOfMonth year m
  Just $ MkDate year m d


||| Helper function to implement some popular interfaces.
unpack : Date -> (Nat, Nat, Nat)
unpack (MkDate year month day) = (year, finToNat month, finToNat day)


export
Eq Date where
  x == y = (unpack x) == (unpack y)

export
Ord Date where
  compare x y = compare (unpack x) (unpack y)

export
Show Date where
  show d =
    let (y, m, d) = unpack d
    -- remember, internal month and day are 0-indexed
    in "\{show y}-\{show (S m)}-\{show (S d)}"

export
ToJSON Date where
  toJSON d = string $ show d

parseDate : Parser String Date
parseDate s = case forget $ split ('-' ==) s of
  [y, m, d] => case date (stringToNatOrZ y) (stringToNatOrZ m) (stringToNatOrZ d) of
      Nothing => fail "Invalid date"
      Just d  => Right d
  _         => fail "Too many fields"

export
FromJSON Date where
  fromJSON = withString "Date" parseDate


export
today : IO Date
today = do
  ct <- clockTime UTC
  let s = seconds ct
  putStrLn $ show s
  pure $ MkDate 2023 8 25
