module Main

import Data.AVL.Dict
import Data.AVL.Set

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
data Ingredient = I String Quantity


-- A recipe is a named list of Ingredients, yielding one or more servings
record Recipe where
  constructor Rx
  name: String
  servings: Nat
  ingredients: List Ingredient

Eq Recipe where
  x == y = (name x) == (name y)

Ord Recipe where
  compare x y = compare (name x) (name y)

{- Global Foods Database -}


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

total
recipes : String -> Recipe
recipes n@"French Toast" = Rx n 1 [
  I "Whole Egg"   (Ea 2.0),
  I "Bread Slice" (Ea 2.0)
]
-- https://youtu.be/L76XJqz9PWo
recipes n@"Shakshuka" = Rx n 2 [
]
recipes n@"Toaster Pizza" = Rx n 1 [
  I "Tortilla"        (Ea         1.0),
  I "Pizza Sauce"     (Vol (Tblsp 1.5)),
  I "Mozarella"       (Wt  (Oz    3.0)),
  I "Italian Sausage" (Wt  (Oz    1.5))
]
-- https://www.thespruceeats.com/stuffed-eggplant-little-shoes-1705821
recipes n@"Greek Stuffed Eggplant" = Rx n 6 [
  I "Small Eggplants"   (Ea          6.0),
  I "Ground Beef"       (Wt  (Lb     0.5)),
  I "Garlic Clove"      (Ea          2.0),
  I "Medium Onion"      (Ea          1.0),
  I "Parsley"           (Vol (C      2.0)),
  I "Olive Oil"         (Vol (Tblsp 10.0)),
  I "Grated Parmesian"  (Vol (C      1.0)),
  I "Tomato"            (Ea          1.0),
  I "All Purpose Flour" (Vol (Tblsp  2.0)),
  I "Butter"            (Vol (Tblsp  2.0)),
  I "Milk"              (Vol (C      1.0)),
  I "Ground Nutmeg"     (Vol (ML     0.5))
]
-- https://www.thespruceeats.com/creole-shrimp-etouffee-3060807
recipes n@"Creole Shrimp Etouffee" = Rx n 6 [
  I "Butter"                       (Vol (Tblsp  7.0)),
  I "All Purpose Flour"            (Vol (Tblsp  6.0)),
  I "Large Onion"                  (Ea          1.0),
  I "Celery"                       (Vol (C      1.5)),
  I "Green Bell Pepper"            (Vol (C      1.0)),
  I "Garlic Clove"                 (Ea          3.0),
  I "Shrimp Stock / Clamp Juice"   (Vol (Floz   8.0)),
  I "Diced Tomatoes"               (Vol (Floz  14.5)),
  I "Creole Seasoning (Salt Free)" (Vol (Tblsp  2.0)),
  I "Black Pepper"                 (Vol (Tsp    0.25)),
  I "Bay Leaf"                     (Ea          1.0),
  I "Shrimp (Peeled and Deveined)" (Wt  (Lb     1.5)),
  I "Cooked Rice"                  (Vol (C      2.0))
]
-- https://cooking.nytimes.com/recipes/1021339-ramen-with-charred-scallions-green-beans-and-chile-oil
recipes n@"Ramen with Scallions" = Rx n 4 [
  I "Red Pepper Flakes"            (Vol (Tblsp  2.0)),
  I "Kosher Salt"                  (Vol (Tsp    1.5)),
  I "Grapeseed Oil"                (Vol (C      0.5)),
  I "Ginger Root"                  (Ea          0.5),
  I "Garlic Clove"                 (Ea          2.0),
  I "Sesame Seeds"                 (Vol (Tsp    2.0)),
  I "Sesame Oil"                   (Vol (Tsp    1.0)),
  I "Kosher Salt"                  (Vol (ML     0.0)),
  I "Ramen Noodles"                (Wt  (Oz    12.0)),
  I "Scallion"                     (Ea         12.0),
  I "Grapeseed Oil"                (Vol (Tblsp  3.0)),
  I "Green Beans"                  (Wt  (Oz    10.0)),
  I "Ginger Root"                  (Ea          0.5),
  I "White Pepper"                 (Wt  (Gram   0.0)),
  I "Sesame Seeds"                 (Vol (Tblsp  1.0))
]
-- https://cooking.nytimes.com/recipes/1022479-sheet-pan-gnocchi-with-mushrooms-and-spinach
recipes n@"Sheet Pan Gnocci" = Rx n 4 [
  I "Mixed Mushrooms"              (Wt  (Lb     1.0)),
  I "Potato Gnocci"                (Wt  (Oz    18.0)),
  I "Olive Oil"                    (Vol (Tblsp  6.0)),
  I "Scallion"                     (Ea          4.0),
  I "Large Shallot"                (Ea          1.0),
  I "Baby Spinach"                 (Wt  (Oz     5.0)),
  I "Dijon Mustartd"               (Vol (Tblsp  2.0)),
  I "Horseradish"                  (Vol (Tblsp  2.0)),
  I "Honey"                        (Vol (Tsp    1.0)),
  I "Butter (Unsalted)"            (Vol (Tblsp  1.0))
]
-- https://cooking.nytimes.com/recipes/1021842-jamaican-curry-chicken-and-potatoes
recipes n@"Jamaican Curry Chicken" = Rx n 4 [
  I "Chicken Thighs"               (Wt  (Lb     3.0)),
  I "Garlic Powder"                (Vol (Tblsp  4.0)),
  I "Kosher Salt"                  (Vol (Tsp    2.0)),
  I "Olive Oil"                    (Vol (Tblsp  2.0)),
  I "Large Onion"                  (Ea          1.0),
  I "Garlic Clove"                 (Ea          4.0),
  I "Jamaican Curry Powder"        (Vol (Tblsp  2.0)),
  I "Scotch Bonnet"                (Ea          1.0),
  I "Medium Yukon Gold Potato"     (Ea          4.0),
  I "Chicken Stock"                (Vol (Qt     2.0)),
  I "Bay Leaf"                     (Ea          1.0),
  I "Fresh Time Sprig"             (Ea          2.0),
  I "Cornstarch"                   (Vol (C      1.0)),
  I "Cooked White Rice"            (Vol (C      2.0))
]
-- https://cooking.nytimes.com/recipes/1021066-chile-crisp-shrimp-and-green-beans
recipes n@"Chile-Crisp Shrimp" = Rx n 4 [
  I "Low Sodium Soy Sauce"         (Vol (Tblsp  1.0)),
  I "Granulated Sugar"             (Vol (Tsp    1.0)),
  I "Red Pepper Flakes"            (Vol (Tsp    1.0)),
  I "Ground Cumin"                 (Vol (Tsp    0.75)),
  I "Shrimp (Peeled and Deveined)" (Wt  (Lb     1.0)),
  I "Kosher Salt"                  (Vol (Tsp    1.0)),
  I "Black Pepper"                 (Vol (Tsp    1.0)),
  I "Garlic Clove"                 (Ea          4.0),
  I "Cinnamon Stick"               (Ea          1.0),
  I "Green Beans"                  (Wt  (Oz    10.0)),
  I "Roasted, Salted Peanuts"      (Vol (C      0.25))
]
-- https://cooking.nytimes.com/recipes/8135-roasted-cod-and-potatoes
recipes n@"Roasted Cod and Potatoes" = Rx n 4 [
  I "Medium Potatoes"              (Ea          5.0),
  I "Butter"                       (Vol (Tblsp  6.0)),
  I "Cod Fillets"                  (Wt  (Lb     1.5))
]
-- https://www.pbs.org/food/recipes/mapo-tofu/
recipes n@"Mapo Tofu" = Rx n 3 [
  I "Low Sodium Chicken Broth"     (Vol (C      0.5)),
  I "Corn Starch"                  (Vol (Tsp    2.0)),
  I "Sugar"                        (Vol (Tsp    1.0)),
  I "Sesame Oil"                   (Vol (Tblsp  1.0)),
  I "Garlic Clove"                 (Ea          2.0),
  I "Scallion"                     (Ea          4.0),
  I "Black Bean Paste"             (Vol (Tblsp  1.0)),
  I "Sichuan Peppercorn"           (Vol (Tsp    0.5)),
  I "Ground Pork"                  (Wt  (Oz     6.0)),
  I "Doubanjiang"                  (Vol (Tsp    2.0)),
  I "Silken Tofu"                  (Wt  (Oz    14.0))
]
recipes _ = Rx "Unknown" 0 []


{- Meal Planning -}


-- total
-- shoppingList
--   :  List Recipe
--   -> Dict String Quantity
-- shoppingList      [] = Dict.empty
-- shoppingList r :: rs = sumAmounts (ingredients r) (shoppingList rs)
-- where
--     :  List  String Quantity
--     -> Ingredient
--     -> Dict String Quantity
--     sumAmounts accum (I food amount) = case lookup food accum of
--                Nothing => insert food amount                 accum
--                        Just a  => insert food (addQuantity a amount) accum

--   -- for some reason foldl loops forever, and I had to write this fold by hand.
--   -- maybe I should be using foldr? do not know.
--   mergeRec : List Ingredient -> Dict String Quantity -> Dict String Quantity
--   mergeRec []        ret = ret
--   mergeRec (x :: xs) ret = (sumAmounts (mergeRec xs ret) x)


-- Mnemonic form for meals
data Meal
  = Breakfast
  | Lunch
  | Dinner
  | Other String

Eq Meal where
  Breakfast == Breakfast = True
  Lunch     == Lunch     = True
  Dinner    == Dinner    = True
  (Other x) == (Other y) = x == y
  _         == _         = False

Ord Meal where
  compare Breakfast Breakfast = EQ
  compare Breakfast _         = LT
  compare Lunch     Breakfast = GT
  compare Lunch     Lunch     = EQ
  compare Lunch     _         = LT
  compare Dinner    Breakfast = GT
  compare Dinner    Lunch     = GT
  compare Dinner    Dinner    = EQ
  compare Dinner    _         = LT
  compare (Other x) (Other y) = compare x y
  compare (Other x) _         = GT


-- The usual days of the week on the western calendar
data Day
  = Sun
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat

asInt : Day -> Nat
asInt Sun = 0
asInt Mon = 1
asInt Tue = 2
asInt Wed = 3
asInt Thu = 4
asInt Fri = 5
asInt Sat = 6

Eq Day where
  x == y = (asInt x) == (asInt y)

Ord Day where
  compare x y = compare (asInt x) (asInt y)

-- a meal plan for a given week
data MealPlan : (calories : Nat) -> Type where
  MkPlan
     : (dishes : Set Recipe)
    -> (menu : Dict (Day, Meal) (Maybe (String, Nat)))
    -> MealPlan calories

-- return total number of servings for the plan
total
totalServings : MealPlan cals -> Nat
totalServings (MkPlan dishes _) = sum (map servings (Set.toList dishes))

-- return the number of servings that have been assigned to a menu
total
assignedServings : MealPlan cals -> Nat
assignedServings (MkPlan _ menu) = foldl addServing 0 menu
  where
    addServing : Nat -> Maybe (String, Nat) -> Nat
    addServing accum Nothing = accum
    addServing accum (Just (_, servings)) = accum + servings

-- return the number of servings not assigned to the menu
-- can't return nat here: we must prove that assignedServings <= totalServings!
-- I'm sure it can be done, but I am not sure how
total
unassignedServings : MealPlan cals -> Int
unassignedServings plan = (cast (totalServings plan)) - (cast (assignedServings plan))

total
accumServings : Day -> Dict String Nat -> ((Day, Meal), Maybe (String, Nat)) -> Dict String Nat
accumServings day accum ((d, m), Just (r, s)) =
   if d == day
   then case (lookup r accum) of
     Nothing => accum
     Just servings => insert r (servings + s) accum
   else accum
accumServings day accum ((d, m), Nothing) = accum

-- return the list of assigned servings for a given day in the plan
total
servingsPerDay : MealPlan cals -> Day -> Dict String Nat
servingsPerDay (MkPlan _ menu) day = foldl (accumServings day) Dict.empty (Dict.toList menu)

{-- user input --}

-- the interaction is based on servings and calories:

-- we start off with an empty plan
total
newPlan : (cals: Nat) -> MealPlan cals
newPlan _ = MkPlan Set.empty Dict.empty

-- we can specify which days and meals we are planning for, and our
-- daily calorie budget.

-- we can choose meals from the database and add them to the plan this
-- will increase our serving / calorie budget. we can also remove them
-- from the plan.

-- we keep doing this until we have enough servings / calories to
-- satisfy the plan

-- we can assign servings from each recipe to a given meal slot
-- we keep doing this until every serving is assigned
