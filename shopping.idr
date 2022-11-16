module Main

-- list of future enhancements
-- - convert volumetric units to mass, and back
-- - interval arithmetic on ingredients and serving sizes
-- - calculate nutritional info per serving of each recipe


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


{- Recipes -}


-- How a food is used within a given recipe
record Ingredient where
 constructor Ing
 food: String
 amount: Quantity


-- A recipe is a named list of Ingredients, yielding one or more servings
record Recipe where
  constructor Rx
  name: String
  servings: Nat
  ingredients: List Ingredient


{- Global Foods Database -}


-- Map food name to the most common unit in which a food is sold
total
measure : String -> Measure
measure n@"Whole Egg"         = ByEach
measure n@"Bread Slice"       = ByEach
measure n@"Butter"            = ByWeight
measure n@"Pizza Sauce"       = ByVolume
measure n@"Mozarella"         = ByWeight
measure n@"Italian Sausage"   = ByWeight
measure n@"Tortilla"          = ByEach
measure n@"Red Pepper Flakes" = ByVolume
measure n@"Ginger Root"       = ByEach
measure n@"White Pepper"      = ByWeight
measure _ = Error


-- Map food name to preferred source
total
sources : String -> List String
sources n@"Whole Egg"         = []
sources n@"Bread Slice"       = ["Grocery Outlet"]
sources n@"Butter"            = []
sources n@"Pizza Sauce"       = ["Scratch"]
sources n@"Mozarella"         = ["Grocery Outlet"]
sources n@"Italian Sausage"   = ["Longs", "MoC"]
sources n@"Tortilla"          = ["TJs", "Grocery Outlet"]
sources n@"Red Pepper Flakes" = []
sources n@"Ginger Root"       = ["Sunrise", "MoC"]
sources n@"White Pepper"      = ["Winco", "MoC", "Whole Foods"]
sources _                     = []


{- Global Recipes Database -}


recipes : String -> Recipe
recipes n@"French Toast" = Rx n 1 [
  Ing "Whole Egg"   (Ea 2.0),
  Ing "Bread Slice" (Ea 2.0)
]
recipes n@"Toaster Pizza" = Rx n 1 [
  Ing "Tortilla"        (Ea         1.0),
  Ing "Pizza Sauce"     (Vol (Tblsp 1.5)),
  Ing "Mozarella"       (Wt  (Oz    3.0)),
  Ing "Italian Sausage" (Wt  (Oz    1.5))
]
-- https://www.thespruceeats.com/stuffed-eggplant-little-shoes-1705821
recipes n@"Greek Stuffed Eggplant" = Rx n 6 [
  Ing "Small Eggplants"   (Ea          6.0),
  Ing "Ground Beef"       (Wt  (Lb     0.5)),
  Ing "Garlic Clove"      (Ea          2.0),
  Ing "Medium Onion"      (Ea          1.0),
  Ing "Parsley"           (Vol (C      2.0)),
  Ing "Olive Oil"         (Vol (Tblsp 10.0)),
  Ing "Grated Parmesian"  (Vol (C      1.0)),
  Ing "Tomato"            (Ea          1.0),
  Ing "All Purpose Flour" (Vol (Tblsp  2.0)),
  Ing "Butter"            (Vol (Tblsp  2.0)),
  Ing "Milk"              (Vol (C      1.0)),
  Ing "Ground Nutmeg"     (Vol (ML     0.5))
]
-- https://www.thespruceeats.com/creole-shrimp-etouffee-3060807
recipes n@"Creole Shrimp Etouffee" = Rx n 6 [
  Ing "Butter"                       (Vol (Tblsp  7.0)),
  Ing "All Purpose Flour"            (Vol (Tblsp  6.0)),
  Ing "Large Onion"                  (Ea          1.0),
  Ing "Celery"                       (Vol (C      1.5)),
  Ing "Green Bell Pepper"            (Vol (C      1.0)),
  Ing "Garlic Clove"                 (Ea          3.0),
  Ing "Shrimp Stock / Clamp Juice"   (Vol (Floz   8.0)),
  Ing "Diced Tomatoes"               (Vol (Floz  14.5)),
  Ing "Creole Seasoning (Salt Free)" (Vol (Tblsp  2.0)),
  Ing "Black Pepper"                 (Vol (Tsp    0.25)),
  Ing "Bay Leaf"                     (Ea          1.0),
  Ing "Shrimp (Peeled and Deveined)" (Wt  (Lb     1.5)),
  Ing "Cooked Rice"                  (Vol (C      2.0))
]
-- https://cooking.nytimes.com/recipes/1021339-ramen-with-charred-scallions-green-beans-and-chile-oil
recipes n@"Ramen with Scallions" = Rx n 4 [
  Ing "Red Pepper Flakes"            (Vol (Tblsp  2.0)),
  Ing "Kosher Salt"                  (Vol (Tsp    1.5)),
  Ing "Grapeseed Oil"                (Vol (C      0.5)),
  Ing "Ginger Root"                  (Ea          0.5),
  Ing "Garlic Clove"                 (Ea          2.0),
  Ing "Sesame Seeds"                 (Vol (Tsp    2.0)),
  Ing "Sesame Oil"                   (Vol (Tsp    1.0)),
  Ing "Kosher Salt"                  (Vol (ML     0.0)),
  Ing "Ramen Noodles"                (Wt  (Oz    12.0)),
  Ing "Scallion"                     (Ea         12.0),
  Ing "Grapeseed Oil"                (Vol (Tblsp  3.0)),
  Ing "Green Beans"                  (Wt  (Oz    10.0)),
  Ing "Ginger Root"                  (Ea          0.5),
  Ing "White Pepper"                 (Wt  (Gram   0.0)),
  Ing "Sesame Seeds"                 (Vol (Tblsp  1.0))
]
-- https://cooking.nytimes.com/recipes/1022479-sheet-pan-gnocchi-with-mushrooms-and-spinach
recipes n@"Sheet Pan Gnocci" = Rx n 4 [
  Ing "Mixed Mushrooms"              (Wt  (Lb     1.0)),
  Ing "Potato Gnocci"                (Wt  (Oz    18.0)),
  Ing "Olive Oil"                    (Vol (Tblsp  6.0)),
  Ing "Scallion"                     (Ea          4.0),
  Ing "Large Shallot"                (Ea          1.0),
  Ing "Baby Spinach"                 (Wt  (Oz     5.0)),
  Ing "Dijon Mustartd"               (Vol (Tblsp  2.0)),
  Ing "Horseradish"                  (Vol (Tblsp  2.0)),
  Ing "Honey"                        (Vol (Tsp    1.0)),
  Ing "Butter (Unsalted)"            (Vol (Tblsp  1.0))
]
-- https://cooking.nytimes.com/recipes/1021842-jamaican-curry-chicken-and-potatoes
recipes n@"Jamaican Curry Chicken" = Rx n 4 [
  Ing "Chicken Thighs"               (Wt  (Lb     3.0)),
  Ing "Garlic Powder"                (Vol (Tblsp  4.0)),
  Ing "Kosher Salt"                  (Vol (Tsp    2.0)),
  Ing "Olive Oil"                    (Vol (Tblsp  2.0)),
  Ing "Large Onion"                  (Ea          1.0),
  Ing "Garlic Clove"                 (Ea          4.0),
  Ing "Jamaican Curry Powder"        (Vol (Tblsp  2.0)),
  Ing "Scotch Bonnet"                (Ea          1.0),
  Ing "Medium Yukon Gold Potato"     (Ea          4.0),
  Ing "Chicken Stock"                (Vol (Qt     2.0)),
  Ing "Bay Leaf"                     (Ea          1.0),
  Ing "Fresh Time Sprig"             (Ea          2.0),
  Ing "Cornstarch"                   (Vol (C      1.0)),
  Ing "Cooked White Rice"            (Vol (C      2.0))
]
-- https://cooking.nytimes.com/recipes/1021066-chile-crisp-shrimp-and-green-beans
recipes n@"Chile-Crisp Shrimp" = Rx n 4 [
  Ing "Low Sodium Soy Sauce"         (Vol (Tblsp  1.0)),
  Ing "Granulated Sugar"             (Vol (Tsp    1.0)),
  Ing "Red Pepper Flakes"            (Vol (Tsp    1.0)),
  Ing "Ground Cumin"                 (Vol (Tsp    0.75)),
  Ing "Shrimp (Peeled and Deveined)" (Wt  (Lb     1.0)),
  Ing "Kosher Salt"                  (Vol (Tsp    1.0)),
  Ing "Black Pepper"                 (Vol (Tsp    1.0)),
  Ing "Garlic Clove"                 (Ea          4.0),
  Ing "Cinnamon Stick"               (Ea          1.0),
  Ing "Green Beans"                  (Wt  (Oz    10.0)),
  Ing "Roasted, Salted Peanuts"      (Vol (C      0.25))
]
-- https://cooking.nytimes.com/recipes/8135-roasted-cod-and-potatoes
recipes n@"Roasted Cod and Potatoes" = Rx n 4 [
  Ing "Medium Potatoes"              (Ea          5.0),
  Ing "Butter"                       (Vol (Tblsp  6.0)),
  Ing "Cod Fillets"                  (Wt  (Lb     1.5))
]
-- https://www.pbs.org/food/recipes/mapo-tofu/
recipes n@"Mapo Tofu" = Rx n 3 [
  Ing "Low Sodium Chicken Broth"     (Vol (C      0.5)),
  Ing "Corn Starch"                  (Vol (Tsp    2.0)),
  Ing "Sugar"                        (Vol (Tsp    1.0)),
  Ing "Sesame Oil"                   (Vol (Tblsp  1.0)),
  Ing "Garlic Clove"                 (Ea          2.0),
  Ing "Scallion"                     (Ea          4.0),
  Ing "Black Bean Paste"             (Vol (Tblsp  1.0)),
  Ing "Sichuan Peppercorn"           (Vol (Tsp    0.5)),
  Ing "Ground Pork"                  (Wt  (Oz     6.0)),
  Ing "Doubanjiang"                  (Vol (Tsp    2.0)),
  Ing "Silken Tofu"                  (Wt  (Oz    14.0))
]


{- Meal Planning -}


-- find the given ingredient within a list of ingredients
total
findIng : Ingredient -> List Ingredient -> Maybe Ingredient
findIng x xs = find (matches x) xs
  where matches : Ingredient -> Ingredient -> Bool
        matches x y = (food x) == (food y)

-- aggregate like ingredients
total
merge : List Ingredient -> List Ingredient
merge [] = []
merge (x :: xs) = case findIng x xs of
  Nothing =>               x :: merge xs
  Just x' => (mergeIng x x') :: merge xs
where
  mergeIng : Ingredient -> Ingredient -> Ingredient
  mergeIng (Ing fa aa) (Ing fb ab) = Ing fa (addQuantity aa ab)


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
