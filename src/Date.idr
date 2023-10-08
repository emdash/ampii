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
import Data.Nat
import Data.Refined.Integer
import Data.Refined.Bits8
import Data.Refined.Bits16
import Derive.Prelude
import Derive.Refined
import JSON.Derive
import Language.Reflection.Util
import System.Clock


%default total
%language ElabReflection


||| Returns true if the given year is a leap year.
export
IsLeapYear : (n: Nat) -> Bool
IsLeapYear year =
  let
    mod4   := (modNatNZ year 4   SIsNonZero) == 0
    mod100 := (modNatNZ year 100 SIsNonZero) == 0
    mod400 := (modNatNZ year 400 SIsNonZero) == 0
  in
    mod4 && ((not mod100) || mod400)


||| A month is an 8-bit unsigned integer between 1 and 12, inclusive.
public export
record Month where
  constructor M
  value : Bits8
  {auto 0 valid : FromTo 1 12 value}

namespace Month
  %runElab derive "Month" [Show, Eq, Ord, RefinedInteger]

||| Returns the number of days of the given 1-indexed month
export
DaysOfMonth : Bool -> Month -> Bits8
DaysOfMonth True  (M 2)  = 29
DaysOfMonth False (M 2)  = 28
DaysOfMonth _     (M 4 ) = 30
DaysOfMonth _     (M 6 ) = 30
DaysOfMonth _     (M 9 ) = 30
DaysOfMonth _     (M 11) = 30
DaysOfMonth _     _      = 31

||| Represents valid day of month
public export
record Day (leap : Bool) (m : Month) where
  constructor D
  day : Bits8
  {auto 0 valid : FromTo 1 (DaysOfMonth leap m) day}

%runElab deriveIndexed "Day" [Show,Eq,Ord,ToJSON]

namespace Day
  public export
  refineDay
    :  {b : Bool}
    -> {m : Month}
    -> Bits8
    -> Maybe (Day b m)
  refineDay v = case hdec0 {p = FromTo 1 (DaysOfMonth b m)} v of
    Just0 _  => Just (D v)
    Nothing0 => Nothing

  public export
  fromInteger
    :  {b : Bool}
    -> {m : Month}
    -> (n : Integer)
    -> {auto 0 p : IsJust (refineDay {b} {m} $ cast n)}
    -> Day b m
  fromInteger v = fromJust $ refineDay (cast v)


||| A type that represents a valid Julian Date
|||
||| Day is a refined integer parameterized on the given month and
||| year.
export
record Date where
  constructor MkDate
  year:  Nat
  month: Month
  day:   Day (IsLeapYear year) month


||| Construct a date from user input.
|||
||| This may fail if the month or day fall outside the expected
||| intervals.
pack : Nat -> Bits8 -> Bits8 -> Maybe Date
pack year m d = do
  m <- refineMonth m
  d <- refineDay   d
  Just $ MkDate year m d


||| Helper function to implement some popular interfaces.
unpack : Date -> (Nat, Bits8, Bits8)
unpack (MkDate year (M month) (D day)) = (year, month, day)

export Eq Date  where x == y      = (unpack x) == (unpack y)
export Ord Date where compare x y = compare (unpack x) (unpack y)

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

{-
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
