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


||| A minimal units library for culinary purposes.
|||
||| I don't try to handle arbitrary numeric types: all quantities use
||| double internally.
|||
||| I also don't try to model arbitrary changes of dimensionality: The
||| main use case is conversions between volume and mass.
|||
||| I make no attempt to preserve units across operations. Every
||| operation internally normalizes to the base units.
|||
||| Generating shopping lists requires aggregating quantities of
||| ingredients, when the same ingredient might be specified by
||| weight, by volume, or by each even within the same recipe: all
||| quantities within this domain can be equated to mass
|||
||| For those cooking somewhere besides earth: I perpetuate the usual
||| fallacy of conflating mass and force within the US-customary
||| system, as this is how such units are used in the kitchen.


module Measures


import Data.Either
import Derive.Prelude
import JSON.Derive


%language ElabReflection
%default total


||| A dimension is a measure of some physical quantity.
|||
||| Here we are primarily concerned with:
||| - Scalar
||| - Mass
||| - Volume
||| - Density
|||
||| Densty = Mass / Volume
public export
data Dimension
  = Scalar
  | Mass
  | Volume
  | Density
  | NotImplemented
%runElab derive "Dimension" [Show, Eq, FromJSON, ToJSON]


||| Rules for multiplying quantities
mul : Dimension -> Dimension -> Dimension
mul Volume   Density = Mass
mul Density  Volume  = Mass
mul Scalar   x       = x
mul x        Scalar  = x
mul _        _       = NotImplemented

||| Rules for dividing quantities
div : Dimension -> Dimension -> Dimension
div Scalar Scalar   = Scalar
div x      Scalar   = x
div Mass   Volume   = Density
div Mass   Mass     = Scalar
div Volume Volume   = Scalar
div Density Density = Scalar
div _      _      = NotImplemented


||| The set of units that we support.
|||
||| It's indexed by Dimension, so we can distinguish at the type level
||| between scalars, units of mass, a units of volume, etc.
|||
||| Note that there's deliberately no data constructor for
||| `Unit NotImplemented`
public export
data Unit : Dimension -> Type where
  S           : Unit Scalar
  Ounces      : Unit Mass
  Pounds      : Unit Mass
  Grams       : Unit Mass
  MilliGrams  : Unit Mass
  KiloGrams   : Unit Mass
  Gallons     : Unit Volume
  Quarts      : Unit Volume
  Pints       : Unit Volume
  Cups        : Unit Volume
  FluidOunces : Unit Volume
  TableSpoons : Unit Volume
  TeaSpoons   : Unit Volume
  MilliLiters : Unit Volume
  Liters      : Unit Volume
  GramsPerML  : Unit Density
%runElab deriveIndexed "Unit" [Eq]

||| Ties off this loose end, and helpful to avoid having to implement
||| operations on quantities we don't support.
public export
Uninhabited (Unit NotImplemented) where
  uninhabited x impossible


||| Convert a unit to its short form
public export
{d : Dimension} -> Show (Unit d) where
  show S           = ""
  show Ounces      = "oz"
  show Pounds      = "lb"
  show Grams       = "g"
  show MilliGrams  = "mg"
  show KiloGrams   = "Kg"
  show Gallons     = "Gal"
  show Quarts      = "qt"
  show Pints       = "pt"
  show Cups        = "C"
  show FluidOunces = "floz"
  show TableSpoons = "T"
  show TeaSpoons   = "t"
  show MilliLiters = "mL"
  show Liters      = "L"
  show GramsPerML  = "g/mL"


||| Table of conversion factors expressed in terms of the base unit.
conv : Unit d -> Double
conv S           = 1.0
conv Ounces      = 28.3492
conv Pounds      = 28.3492 * 16
conv Grams       = 1.0
conv MilliGrams  = 0.001
conv KiloGrams   = 1000.0
conv Gallons     = 29.57344 * 128
conv Quarts      = 29.57344 * 32
conv Pints       = 29.57344 * 16
conv Cups        = 29.57344 * 8
conv FluidOunces = 29.57344
conv TableSpoons = 5.0 * 3
conv TeaSpoons   = 5.0
conv MilliLiters = 1.0
conv Liters      = 1000.0
conv GramsPerML  = 1.0


||| An amount of some stuff with an associated unit.
|||
||| Quantities are indexed over the dimensionality of the unit.
public export
record Quantity (d : Dimension) where
  constructor Q
  amount : Double
  unit   : Unit d
%runElab deriveIndexed "Quantity" [Eq]


||| Table of base units for the given dimension
baseUnit : {d : Dimension} -> Unit d
baseUnit {d = Scalar}         = S
baseUnit {d = Mass}           = Grams
baseUnit {d = Volume}         = MilliLiters
baseUnit {d = Density}        = GramsPerML
-- xxx: is there a way to avoid using `idris_crash` here?
-- I believe that there might be, but anyway...
baseUnit {d = NotImplemented} = assert_total $ idris_crash "Not Implemented"

||| Convert the amount given quantity into the base unit for a given dimension.
normalize : {d : Dimension} -> Quantity d -> Quantity d
normalize {d = NotImplemented} x = absurd $ uninhabited x.unit
normalize {d}                  x = Q (conv x.unit * x.amount) (baseUnit {d})

||| Convert a given quantity to the specified unit
|||
||| Use it like: `5.Gal \`as\` Liters`
public export
as : {d : Dimension} -> Quantity d -> Unit d -> Quantity d
as x u = Q ((normalize x).amount / (conv u)) u

||| Multiply two quantities
|||
||| The result type respects the dimensionality of the operands, so we
||| must implement this as a function `(*)`, and not via `Num`.
public export
(*)
  :  {a, b : Dimension}
  -> Quantity a
  -> Quantity b
  -> Quantity (a `mul` b)
(*) {a} {b} x y = Q
  ((normalize x).amount * (normalize y).amount)
  (baseUnit {d = a `mul` b})

||| Type-safe division of quantities
|||
||| The result type respects the dimensionality of the operands, so we
||| must implement this as a function `(/)` and not via `Fractional`.
public export
(/)
  : {a, b : Dimension}
  -> Quantity a
  -> Quantity b
  -> Quantity (a `div` b)
(/) {a} {b} x y = Q
  ((normalize x).amount / (normalize y).amount)
  (baseUnit {d = a `div` b})

||| Type-safe addition of quantities.
|||
||| We can't implement `Num` for quantities, as the behavior of (*)
||| doesn't match the type signature.
public export
(+) : {d : Dimension} -> Quantity d -> Quantity d -> Quantity d
(+) {d} x y = Q ((normalize x).amount + (normalize y).amount) (baseUnit {d})

||| Type-safe subtraction of quantities
|||
||| We *could* implement `Neg` for quantities, but it's unclear what
||| the meaning of `neg` is for the quantities we support.
public export
(-) : {d : Dimension} -> Quantity d -> Quantity d -> Quantity d
(-) {d} x y = Q ((normalize x).amount - (normalize y).amount) (baseUnit {d})

||| Discard unit and convert to a plain double
public export
Cast (Quantity d) Double where
  cast q = q.amount


{- Convenient abbreviations for working with quantities ******************** -}

||| Factor out the type of abreviations to compress the following table
public export
0 Abr : Dimension -> Type ; Abr d = Double -> Quantity d

public export (.oz)   : Abr Mass   ; (.oz)   x = Q x Ounces
public export (.lb)   : Abr Mass   ; (.lb)   x = Q x Pounds
public export (.g)    : Abr Mass   ; (.g)    x = Q x Grams
public export (.mg)   : Abr Mass   ; (.mg)   x = Q x MilliGrams
public export (.Kg)   : Abr Mass   ; (.Kg)   x = Q x KiloGrams
public export (.Gal)  : Abr Volume ; (.Gal)  x = Q x Gallons
public export (.qt)   : Abr Volume ; (.qt)   x = Q x Quarts
public export (.pt)   : Abr Volume ; (.pt)   x = Q x Pints
public export (.C)    : Abr Volume ; (.C)    x = Q x Cups
public export (.floz) : Abr Volume ; (.floz) x = Q x FluidOunces
public export (.T)    : Abr Volume ; (.T)    x = Q x TableSpoons
public export (.t)    : Abr Volume ; (.t)    x = Q x TeaSpoons
public export (.mL)   : Abr Volume ; (.mL)   x = Q x MilliLiters
public export (.L)    : Abr Volume ; (.L)    x = Q x Liters


{- Support JSON Deserialization ******************************************** -}

||| Parse a given unit abbreviation.
|||
||| This is meant to be the
public export
unitFromString : {d : Dimension} -> String -> Maybe (Unit d)
unitFromString {d = Scalar} ""     = Just S
unitFromString {d = Mass  } "oz"   = Just Ounces
unitFromString {d = Mass  } "lb"   = Just Pounds
unitFromString {d = Mass  } "g"    = Just Grams
unitFromString {d = Mass  } "mg"   = Just MilliGrams
unitFromString {d = Mass  } "Kg"   = Just KiloGrams
unitFromString {d = Volume} "Gal"  = Just Gallons
unitFromString {d = Volume} "qt"   = Just Quarts
unitFromString {d = Volume} "pt"   = Just Pints
unitFromString {d = Volume} "C"    = Just Cups
unitFromString {d = Volume} "floz" = Just FluidOunces
unitFromString {d = Volume} "T"    = Just TableSpoons
unitFromString {d = Volume} "t"    = Just TeaSpoons
unitFromString {d = Volume} "mL"   = Just MilliLiters
unitFromString {d = Volume} "L"    = Just Liters
unitFromString              _      = Nothing

||| Parse a quantity as `amount ++ unit`
public export
quantityFromString : {d : Dimension} -> String -> Maybe (Quantity d)
quantityFromString {d} str =
  let (amount, unit) := break (\x => not (isDigit x || (x == '.'))) str
  in case parseDouble amount of
    Nothing => Nothing
    Just x  => Just $ Q x !(unitFromString unit)

public export
{d : Dimension} -> Show (Quantity d) where
  show (Q a u) = "\{show a}\{show u}"

||| Parse a unit from a JSON value
parseUnit : {d : Dimension} -> Parser String (Unit d)
parseUnit u = case (unitFromString u) of
  Nothing => fail "unrecognized unit \{u}"
  Just u  => Right u

||| Parse a quantity from a JSON value
parseQuantity : {d : Dimension} -> Parser String (Quantity d)
parseQuantity q = case (quantityFromString q) of
  Nothing => fail "unrecognized quantity \{q}"
  Just q  => Right q

public export
{d : Dimension} -> FromJSON (Unit d) where
  fromJSON = withString "Unit Mass" parseUnit

public export
{d : Dimension} -> ToJSON (Quantity d) where
  toJSON q = toJSON "\{show q}"

public export
{d : Dimension} -> FromJSON (Quantity d) where
  fromJSON = withString "Quantity \{show d}" parseQuantity
