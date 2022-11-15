module Main


{-
  A minimal units library

  we only need to support addition and multiplication.
-}

-- the type of measurement, independent of unit
data Measure
  = ByWeight
  | ByVolume
  | ByEach
  | Error

-- units of weight
data Weight
  = Oz   Double
  | Lb   Double
  | Gram Double

-- units of volume
data Volume
  = Gal   Double
  | Qt    Double
  | Pt    Double
  | C     Double
  | Floz  Double
  | Tblsp Double
  | Tsp   Double
  | ML    Double

data Quantity
  = Wt   Weight
  | Vol  Volume
  | Ea   Double
  | Err

-- Map between a dimension and the concrete type required
total
measureOf : Quantity -> Measure
measureOf (Wt  x) = ByWeight
measureOf (Vol x) = ByVolume
measureOf (Ea  x) = ByEach
measureOf Err     = Error

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

total
addQuantity : (a: Quantity) -> (b: Quantity) -> Quantity
addQuantity (Wt  x) (Wt  y) = Wt  (addWeight x y)
addQuantity (Vol x) (Vol y) = Vol (addVolume x y)
addQuantity (Ea  x) (Ea  y) = Ea  (x + y)
addQuantity _        _      = Err

total
scaleQuantity : Double -> Quantity -> Quantity
scaleQuantity s (Wt  x) = Wt  (scaleWeight s x)
scaleQuantity s (Vol x) = Vol (scaleVolume s x)
scaleQuantity s (Ea  x) = Ea  (s * x)
scaleQuantity s Err = Err

{- Food and Recipe Database -}

-- Data for a given food
-- This can be expanded to include nutrition info, etc.
record FoodStuff where
  constructor Food
  name: String
  measure: Measure

-- How a food is used within a given recipe
data Ingredient : Type where
 Ing : (food: FoodStuff)
    -> (amount: Quantity)
    -> (preferredSource: String)
    -> Ingredient

-- project food from ingredient
total
food : Ingredient -> FoodStuff
food (Ing food amount preferredSource) = food

-- project amount from ingredient
total
amount : (i: Ingredient) -> Quantity
amount (Ing food amount preferredSource) = amount


{- Global Foods Database -}


total
foods : String -> FoodStuff
foods n@"Whole Egg"         = Food n ByEach
foods n@"Bread Slice"       = Food n ByEach
foods n@"Butter"            = Food n ByWeight
foods n@"Pizza Sauce"       = Food n ByVolume
foods n@"Mozarella"         = Food n ByWeight
foods n@"Italian Sausage"   = Food n ByWeight
foods n@"Tortilla"          = Food n ByEach
foods n@"Red Pepper Flakes" = Food n ByVolume
foods n@"Ginger Root"       = Food n ByEach
foods n@"White Pepper"      = Food n ByWeight
foods _ = Food "Undefined" Error

-- A recipe is a named list of Ingredients
record Recipe where
  constructor Rx
  name: String
  ingredients: List Ingredient

-- Helper function to construct an ingredient in a recipe
ing
  :  (n: String)
  -> (q: Quantity)
  -> (ps: String)
  -> Ingredient
ing n q ps = let
  food = foods n
  mf   = measure   food
  mq   = measureOf q
in ing' mf mq food ps where
  ing' : Measure -> Measure -> FoodStuff -> String -> Ingredient
  ing' ByWeight ByWeight f ps = Ing f q ps
  ing' ByVolume ByVolume f ps = Ing f q ps
  ing' ByEach   ByEach   f ps = Ing f q ps


{- Global Recipes Database -}


recipes : String -> Recipe
recipes n@"French Toast" = Rx n [
  ing "Whole Egg"   (Ea 2.0) "Anywhere",
  ing "Bread Slice" (Ea 2.0) "Anywhere"
]
recipes n@"Toaster Pizza" = Rx n [
  ing "Tortilla"        (Ea         1.0)  "Grocery Outlet",
  ing "Pizza Sauce"     (Vol (Tblsp 1.5)) "Scratch",
  ing "Mozarella"       (Wt  (Oz    3.0)) "Grocery Outlet",
  ing "Italian Sausage" (Wt  (Oz    1.5)) "Winco"
]
-- https://www.thespruceeats.com/stuffed-eggplant-little-shoes-1705821
recipes n@"Greek Stuffed Eggplant" = Rx n [
  ing "Small Eggplants"   (Ea          6.0)  "MoC",
  ing "Ground Beef"       (Wt  (Lb     0.5)) "Longs",
  ing "Garlic Clove"      (Ea          2.0)  "Anywhere",
  ing "Medium Onion"      (Ea          1.0)  "Anywhere",
  ing "Parsley"           (Vol (C      2.0)) "Anywhere",
  ing "Olive Oil"         (Vol (Tblsp 10.0)) "MoC",
  ing "Grated Parmesian"  (Vol (C      1.0)) "Grocery Outlet",
  ing "Tomato"            (Ea          1.0)  "Anywhere",
  ing "All Purpose Flour" (Vol (Tblsp  2.0)) "Anywhere",
  ing "Butter"            (Vol (Tblsp  2.0)) "Anywhere",
  ing "Milk"              (Vol (C      1.0)) "Anywhere",
  ing "Ground Nutmeg"     (Vol (ML     0.5)) "Anywhere"
]
-- https://www.thespruceeats.com/creole-shrimp-etouffee-3060807
recipes n@"Creole Shrimp Etouffee" = Rx n [
  ing "Butter"                       (Vol (Tblsp  7.0))  "Anywhere",
  ing "All Purpose Flour"            (Vol (Tblsp  6.0))  "Anywhere",
  ing "Large Onion"                  (Ea          1.0)   "Anywhere",
  ing "Celery"                       (Vol (C      1.5))  "Anywhere",
  ing "Green Bell Pepper"            (Vol (C      1.0))  "Anywhere",
  ing "Garlic Clove"                 (Ea          3.0)   "Anywhere",
  ing "Shrimp Stock / Clamp Juice"   (Vol (Floz   8.0))  "MoC",
  ing "Diced Tomatoes"               (Vol (Floz  14.5))  "Anywhere",
  ing "Creole Seasoning (Salt Free)" (Vol (Tblsp  2.0))  "Anywhere",
  ing "Black Pepper"                 (Vol (Tsp    0.25)) "Anywhere",
  ing "Bay Leaf"                     (Ea          1.0)   "Anywhere",
  ing "Shrimp (Peeled and Deveined)" (Wt  (Lb     1.5))  "Anywhere",
  ing "Cooked Rice"                  (Vol (C      2.0))  "Anywhere"
]
-- https://cooking.nytimes.com/recipes/1021339-ramen-with-charred-scallions-green-beans-and-chile-oil
recipes n@"Ramen with Scallions"  = Rx n [
  ing "Red Pepper Flakes"            (Vol (Tblsp  2.0)) "Anywhere",
  ing "Kosher Salt"                  (Vol (Tsp    1.5)) "Anywhere",
  ing "Grapeseed Oil"                (Vol (C      0.5)) "Anywhere",
  ing "Ginger Root"                  (Ea          0.5)  "Sunrise",
  ing "Garlic Clove"                 (Ea          2.0)  "Anywhere",
  ing "Sesame Seeds"                 (Vol (Tsp    2.0)) "Anywhere",
  ing "Sesame Oil"                   (Vol (Tsp    1.0)) "Anywhere",
  ing "Kosher Salt"                  (Vol (ML     0.0)) "Anywhere",
  ing "Ramen Noodles"                (Wt  (Oz    12.0)) "Sunrise",
  ing "Scallion"                     (Ea         12.0)  "Anywhere",
  ing "Grapeseed Oil"                (Vol (Tblsp  3.0)) "Anywhere",
  ing "Green Beans"                  (Wt  (Oz    10.0)) "Anywhere",
  ing "Ginger Root"                  (Ea          0.5)  "Sunrise",
  ing "White Pepper"                 (Wt  (Gram   0.0)) "MoC",
  ing "Sesame Seeds (Toasted)"       (Vol (Tblsp  1.0)) "Anywhere"
]
-- https://cooking.nytimes.com/recipes/1022479-sheet-pan-gnocchi-with-mushrooms-and-spinach


{- Meal Planning -}


-- find the given ingredient within a list of ingredients
total
findIng : Ingredient -> List Ingredient -> Maybe Ingredient
findIng x xs = find (matches x) xs
  where matches : Ingredient -> Ingredient -> Bool
        matches x y = (name (food x)) == (name (food y))

-- aggregate like ingredients
total
merge : List Ingredient -> List Ingredient
merge [] = []
merge (x :: xs) = case findIng x xs of
  Nothing =>               x :: merge xs
  Just x' => (mergeIng x x') :: merge xs
where
  mergeIng : Ingredient -> Ingredient -> Ingredient
  mergeIng (Ing fa aa psa) (Ing fb ab psb) = Ing fa (addQuantity aa ab) psb


-- models three square meals a day
record DayPlan where
  constructor Day
  breakfast: List Recipe
  lunch: List Recipe
  dinner: List Recipe
  other: List Recipe

mergeDay : DayPlan -> List Ingredient
mergeDay (Day breakfast lunch dinner other) = let
  meals    = [breakfast, lunch, dinner, other]
  recipes  = foldl (++) [] meals
in
  foldl (++) [] (map ingredients recipes)


mon : DayPlan
mon = Day
  [recipes "French Toast"]
  [recipes "Toaster Pizza"]
  [recipes "Ramen with Scallions"]
  []

tue : DayPlan
tue = Day
  [recipes "French Toast"]
  [recipes "Ramen with Scallions"]
  [recipes "Shrimp Etouffee"]
  []

-- the complete list of ingredients for a given week
shoppingList : List Ingredient
shoppingList = let
  days = [mon, tue]
  ings = foldl (++) [] (map mergeDay days)
in
  Main.merge ings
