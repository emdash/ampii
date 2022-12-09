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
||| The main goal is generating shopping lists from lists of
||| recipes. It's not a general-purpose framework for dimensional
||| analysis.
|||
||| Generating shopping lists requires aggregating quantities of
||| ingredients, when the same ingredient might be specified by
||| weight, by volume, or by each even within the same recipe: all
||| quantities within this domain can be equated to mass, since all
||| foodstuffs have either density or average weight, however, this
||| conversion depends on the foodstuff in question. Feedback is
||| welcome here on whether I've used dependent types effectively.
|||
||| Therefore, internally, all operations on quantities are normalized
||| to mass. This allows any two quantities to be added together. For
||| presentation, normalized quantities are converted to the user's
||| preferred measurement system as both weight *and* volume, since
||| either or both may be useful in a shopping context.
|||
||| For those cooking in low-earth orbit: I perpetuate the usual
||| fallacy of conflating mass and force within the US-customary
||| system, as this is how such units are used in the kitchen.
|||
||| For those cooking in europe: you probably don't need this app to
||| begin with, unless you want to cook from american recipes, in
||| which case, the unit-conversion features will probably be quite
||| helpful.
|||
||| Design-wise: I'm trying to keep the code in this file agnostic
||| w/r/t food-stuffs, with the eventual goal of replacing this
||| library with a general-purpose dimensional-analysis
||| framework. This is more of an exercise in working with dependent
||| types than a pragmatic concern. If I cared about being pragmatic,
||| I wouldn't be writing this in Idris.


module Measures


||| The ways in which normalization can fail
public export data Error
  = UndefinedQuantity
  | UndefinedDensity
  | UndefinedWeight
  | Mismatch a a
  | Union Error Error

||| Sizes of whole ingredients
public export data Size = Small | Med | Large | XLarge | Jumbo

||| How to convert between volume or size + count, and mass.
public export interface Material m where
  density : m ->         Maybe Double
  weights : m -> Size -> Maybe Double

||| A dimensioned quantity, as it would appear in a recipe.
|||
||| Any supported unit is valid here.
public export
data Quantity : m -> Type where
  -- units of weight
  Oz    : m -> Double -> Quantity m
  Lb    : m -> Double -> Quantity m
  Gram  : m -> Double -> Quantity m
  -- units of volume
  Gal   : m -> Double -> Quantity m
  Qt    : m -> Double -> Quantity m
  Pt    : m -> Double -> Quantity m
  C     : m -> Double -> Quantity m
  Floz  : m -> Double -> Quantity m
  Tblsp : m -> Double -> Quantity m
  Tsp   : m -> Double -> Quantity m
  ML    : m -> Double -> Quantity m
  L     : m -> Double -> Quantity m
  -- different sizes of individual items
  Whole : m -> Size -> Double -> Quantity m
  -- error case
  Err   : Error -> Quantity m


-- This namespace avoids some name collisions, but may prove
-- unnecessary in the end
namespace Normalized
  {- conversion factors -}
  total gramsPerOz : Double ; gramsPerOz = 28.34952
  total mlPerFloz  : Double ; mlPerFloz  = 29.57344
  total mlPerTsp   : Double ; mlPerTsp   = 5.0

  ||| Internal quantity type normalizes everything to mass
  data Mass : Type -> Type where
    Gram : Material m => Eq m => m -> Double -> Mass m
    Err  : Error -> Mass m

  ||| Normalize to mass from a volume and material
  total
  fromVolume : Material m => Eq m => m -> Double -> Mass m
  fromVolume what ml = case density what of
    Just density => Gram what (ml * density)
    Nothing      => Err UndefinedDensity

  ||| Normalize to mass from (count, size)
  fromWhole : Material m => Eq m => m -> Size -> Double -> Mass m
  fromWhole what size count = case weights what size of
    Just weight => Gram what (weight * count)
    Nothing     => Err UndefinedWeight

  ||| Convert quantity to a Mass
  toMass : Material m => Eq m => Quantity m -> Mass m
  toMass (Oz      what x)   = Gram       what (x * gramsPerOz)
  toMass (Lb      what x)   = Gram       what (x * gramsPerOz * 16)
  toMass (Gram    what x)   = Gram       what  x
  toMass (Gal     what x)   = fromVolume what (x * mlPerFloz * 128)
  toMass (Qt      what x)   = fromVolume what (x * mlPerFloz *  32)
  toMass (Pt      what x)   = fromVolume what (x * mlPerFloz *  16)
  toMass (C       what x)   = fromVolume what (x * mlPerFloz *   8)
  toMass (Floz    what x)   = fromVolume what (x * mlPerFloz *   1)
  toMass (Tblsp   what x)   = fromVolume what (x * mlPerTsp  *   3)
  toMass (Tsp     what x)   = fromVolume what (x * mlPerTsp  *   1)
  toMass (ML      what x)   = fromVolume what  x
  toMass (L       what x)   = fromVolume what (x * 1000)
  toMass (Whole   what s x) = fromWhole  what s x
  toMass (Err err)          = Normalized.Err err

  ||| Addition of Masses
  total
  add : Eq m => Mass m -> Mass m -> Mass m
  add (Gram this x) (Gram that y)  =
    if   (this == that)
    then Gram this (x + y)
    else Err (Mismatch this that)
  add (Err err)  (Gram _ _) = Err err
  add (Gram _ _) (Err  err) = Err err
  add (Err e1)   (Err  e2)  = Err (Union e1 e2)

  ||| Scaling of Masses
  total
  scale : Double -> Mass m -> Mass m
  scale scale (Gram what x) = Gram what (scale * x)
  scale scale (Err  err)    = Err err

  ||| Convert from Mass to Quantity
  total
  fromMass : Material m => Eq m => Mass m -> Quantity m
  fromMass (Gram what x) = Measures.Gram what x
  fromMass (Err err)     = Measures.Err  err


||| Add two quantities
export total
addQuantity : Material m => Eq m => Quantity m -> Quantity m -> Quantity m
addQuantity x y = fromMass (Normalized.add (toMass x) (toMass y))


||| Scale a quantity by a scalar
export total
scaleQuantity : Material m => Eq m => Double -> Quantity m -> Quantity m
scaleQuantity s x = fromMass (Normalized.scale s (toMass x))
