module Measures


{-
  A minimal units library for culinary purposes.

  we only need to support addition and multiplication of mass and
  volume, and we perpetuate the usual fallacy of conflating mass and
  force within the us customary system. This library is only
  appropriate for cooking on earth. Do not use for science or for
  cooking on mars, etc.
-}

-- the type of measurement, independent of unit
public export
data Measure
  = ByWeight
  | ByVolume
  | ByEach
  | Error

-- units of weight
public export
data Weight
  = Oz   Double
  | Lb   Double
  | Gram Double

-- units of volume
public export
data Volume
  = Gal   Double
  | Qt    Double
  | Pt    Double
  | C     Double
  | Floz  Double
  | Tblsp Double
  | Tsp   Double
  | ML    Double

public export
data Quantity
  = Wt   Weight
  | Vol  Volume
  | Ea   Double
  | Err


total
ozToGram : Double -> Double
ozToGram x = 28.34952 * x

total
normalizeWeight : Weight -> Double
normalizeWeight (Oz   x) = ozToGram x
normalizeWeight (Lb   x) = (ozToGram x) * 16
normalizeWeight (Gram x) = x

total
addWeight : Weight -> Weight -> Weight
addWeight x y = Gram ((normalizeWeight x) + (normalizeWeight y))

total
scaleWeight : Double -> Weight -> Weight
scaleWeight s w = Gram (s * (normalizeWeight w))

total
flozToML : Double -> Double
flozToML x = 29.57344 * x

total
tspToML : Double -> Double
tspToML x = 5 * x

total
normalizeVolume : Volume -> Double
normalizeVolume (Gal   x) = (flozToML x) * 128
normalizeVolume (Qt    x) = (flozToML x) *  32
normalizeVolume (Pt    x) = (flozToML x) *  16
normalizeVolume (C     x) = (flozToML x) *   8
normalizeVolume (Floz  x) = (flozToML x)
normalizeVolume (Tblsp x) = (tspToML  x) *   3
normalizeVolume (Tsp   x) = (tspToML  x)
normalizeVolume (ML    x) = x

total
addVolume : Volume -> Volume -> Volume
addVolume x y = ML ((normalizeVolume x) + (normalizeVolume y))

total
scaleVolume : Double -> Volume -> Volume
scaleVolume s x = ML (s * (normalizeVolume x))

export total
addQuantity : (a: Quantity) -> (b: Quantity) -> Quantity
addQuantity (Wt  x) (Wt  y) = Wt  (addWeight x y)
addQuantity (Vol x) (Vol y) = Vol (addVolume x y)
addQuantity (Ea  x) (Ea  y) = Ea  (x + y)
addQuantity _        _      = Err

export total
scaleQuantity : Double -> Quantity -> Quantity
scaleQuantity s (Wt  x) = Wt  (scaleWeight s x)
scaleQuantity s (Vol x) = Vol (scaleVolume s x)
scaleQuantity s (Ea  x) = Ea  (s * x)
scaleQuantity s Err     = Err
