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
||| Density = Mass / Volume
public export
data Dimension
  = Scalar
  | Mass
  | Volume
  | Density
%runElab derive "Dimension" [Show, Eq, FromJSON, ToJSON]


||| Static rules for multiplication of dimensions.
data Mul : (a : Dimension) -> (b : Dimension) -> Dimension -> Type where
  [search a b]
  Mr1 : Mul Volume  Density Mass
  Mr2 : Mul Density Volume  Mass
  Mr3 : Mul Scalar  x       x
  Mr4 : Mul x       Scalar  x

||| Static rules for division of dimensions.
data Div : (a : Dimension) -> (b : Dimension) -> Dimension -> Type where
  [search a b]
  Dr1 : Div Scalar  Scalar  Scalar
  Dr2 : Div x       Scalar  x
  Dr3 : Div Mass    Volume  Density
  Dr4 : Div Mass    Mass    Scalar
  Dr5 : Div Volume  Volume  Scalar
  Dr6 : Div Density Density Scalar


||| The set of units that we support.
|||
||| It's indexed by Dimension, so we can distinguish at the type level
||| between scalars, units of mass, a units of volume, etc.
|||
||| Note that there's deliberately no data constructor for
||| `Unit NotImplemented`
public export
data UnitT : Dimension -> Type where
  S           : UnitT Scalar
  Ounces      : UnitT Mass
  Pounds      : UnitT Mass
  Grams       : UnitT Mass
  MilliGrams  : UnitT Mass
  KiloGrams   : UnitT Mass
  Gallons     : UnitT Volume
  Quarts      : UnitT Volume
  Pints       : UnitT Volume
  Cups        : UnitT Volume
  FluidOunces : UnitT Volume
  TableSpoons : UnitT Volume
  TeaSpoons   : UnitT Volume
  MilliLiters : UnitT Volume
  Liters      : UnitT Volume
  GramsPerML  : UnitT Density
%runElab deriveIndexed "UnitT" [Eq, Ord]

||| Get the list of supported units of mass.
public export
Units : {d : Dimension} -> List (UnitT d)
Units {d = Scalar} = [S]
Units {d = Mass} = [
  Ounces,
  Pounds,
  Grams,
  MilliGrams,
  KiloGrams
]
Units {d = Volume} = [
  Gallons,
  Quarts,
  Pints,
  Cups,
  FluidOunces,
  TableSpoons,
  TeaSpoons,
  MilliLiters,
  Liters
]
Units {d = Density} = [GramsPerML]

||| XXX: this feels really boiler-platey, need some meta-programming
||| solution for this.
|||
||| Perhaps via the `finite` package.
export
%hint
0 unitInUnits
  :  {auto 0 d : Dimension}
  -> {auto 0 u : UnitT d}
  -> IsJust (findIndex (u ==) Units)
unitInUnits {d = Scalar, u = S} = ItIsJust
unitInUnits {d = Mass, u = Ounces} = ItIsJust
unitInUnits {d = Mass, u = Pounds} = ItIsJust
unitInUnits {d = Mass, u = Grams} = ItIsJust
unitInUnits {d = Mass, u = MilliGrams} = ItIsJust
unitInUnits {d = Mass, u = KiloGrams} = ItIsJust
unitInUnits {d = Volume, u = Gallons} = ItIsJust
unitInUnits {d = Volume, u = Quarts} = ItIsJust
unitInUnits {d = Volume, u = Pints} = ItIsJust
unitInUnits {d = Volume, u = Cups} = ItIsJust
unitInUnits {d = Volume, u = FluidOunces} = ItIsJust
unitInUnits {d = Volume, u = TableSpoons} = ItIsJust
unitInUnits {d = Volume, u = TeaSpoons} = ItIsJust
unitInUnits {d = Volume, u = MilliLiters} = ItIsJust
unitInUnits {d = Volume, u = Liters} = ItIsJust
unitInUnits {d = Density, u = GramsPerML} = ItIsJust

||| Convert a unit to its short form
public export
{0 d : Dimension} -> Show (UnitT d) where
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

||| An amount of some stuff with an associated unit.
|||
||| Quantities are indexed over the dimensionality of the unit.
public export
record Quantity (d : Dimension) where
  constructor Q
  amount : Double
  unit   : UnitT d
%runElab deriveIndexed "Quantity" [Eq, Ord]

||| Table of conversion factors expressed in terms of the base unit.
conv : UnitT d -> Double
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

||| Table of base units for the given dimension
baseUnit : {d : Dimension} -> UnitT d
baseUnit {d = Scalar}  = S
baseUnit {d = Mass}    = Grams
baseUnit {d = Volume}  = MilliLiters
baseUnit {d = Density} = GramsPerML

||| Convert the amount given quantity into the base unit for a given dimension.
normalize : {d : Dimension} -> Quantity d -> Quantity d
normalize (Q amount S) = Q amount S
normalize (Q amount u) = Q (conv u * amount) baseUnit

||| Convert a given quantity to the specified unit
|||
||| Use it like: `5.Gal \`as\` Liters`
public export
as : {d : Dimension} -> Quantity d -> UnitT d -> Quantity d
as x u = Q ((normalize x).amount / (conv u)) u

||| Multiply two quantities
|||
||| The result type respects the dimensionality of the operands, so we
||| must implement this as a function `(*)`, and not via `Num`.
public export
(*)
  :  {a, b, r : Dimension}
  -> Mul a b r
  => Quantity a
  -> Quantity b
  -> Quantity r
(*) {a} {b} x y = Q ((normalize x).amount * (normalize y).amount) baseUnit

||| Type-safe division of quantities
|||
||| The result type respects the dimensionality of the operands, so we
||| must implement this as a function `(/)` and not via `Fractional`.
public export
(/)
  : {a, b, r : Dimension}
  -> Div a b r
  => Quantity a
  -> Quantity b
  -> Quantity r
(/) {a} {b} x y = Q ((normalize x).amount / (normalize y).amount) baseUnit

||| Type-safe addition of quantities.
|||
||| We can't implement `Num` for quantities, as the behavior of (*)
||| doesn't match the type signature.
public export
(+) : {d : Dimension} -> Quantity d -> Quantity d -> Quantity d
(+) x y = Q ((normalize x).amount + (normalize y).amount) baseUnit

||| Type-safe subtraction of quantities
|||
||| We *could* implement `Neg` for quantities, but it's unclear what
||| the meaning of `neg` is for the quantities we support.
public export
(-) : {d : Dimension} -> Quantity d -> Quantity d -> Quantity d
(-) x y = Q ((normalize x).amount - (normalize y).amount) baseUnit

||| Discard unit and convert to a plain double
public export
Cast (Quantity d) Double where cast q = q.amount


{- Convenient abbreviations for working with quantities ******************** -}

||| Factor out the type of abreviations to compress the following table
public export
0 Abr : Dimension -> Type ; Abr d = Double -> Quantity d

public export (.whole): Abr Scalar   ; (.whole) x = Q x S
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
public export
unitFromString : {d : Dimension} -> String -> Maybe (UnitT d)
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

||| Parse a quantity as `amount ++ unit`.
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

||| Parse a unit from a JSON value.
parseUnit : {d : Dimension} -> Parser String (UnitT d)
parseUnit u = case (unitFromString u) of
  Nothing => fail "unrecognized unit \{u}"
  Just u  => Right u

||| Parse a quantity from a JSON value.
parseQuantity : {d : Dimension} -> Parser String (Quantity d)
parseQuantity q = case (quantityFromString q) of
  Nothing => fail "unrecognized quantity \{q}"
  Just q  => Right q

public export
{d : Dimension} -> FromJSON (UnitT d) where
  fromJSON = withString "Unit \{show d}" parseUnit

public export
{d : Dimension} -> ToJSON (Quantity d) where
  toJSON q = toJSON "\{show q}"

public export
{d : Dimension} -> FromJSON (Quantity d) where
  fromJSON = withString "Quantity \{show d}" parseQuantity

||| Short-hand for `Quantity Mass`.
public export
0 Weight : Type
Weight = Quantity Mass
