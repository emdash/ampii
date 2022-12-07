module Plan

import Data.AVL.Dict
import Data.AVL.Set
import Recipes

-- Mnemonic form for meals


public export data Meal
  = Breakfast
  | Lunch
  | Dinner
  | Other String

export
Eq Meal where
  Breakfast == Breakfast = True
  Lunch     == Lunch     = True
  Dinner    == Dinner    = True
  (Other x) == (Other y) = x == y
  _         == _         = False

export
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
public export
data Day
  = Sun
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat

export
asInt : Day -> Nat
asInt Sun = 0
asInt Mon = 1
asInt Tue = 2
asInt Wed = 3
asInt Thu = 4
asInt Fri = 5
asInt Sat = 6

export
Eq Day where
  x == y = (asInt x) == (asInt y)

export
Ord Day where
  compare x y = compare (asInt x) (asInt y)

-- a meal plan for a given week
export
data MealPlan : (calories : Nat) -> Type where
  MkPlan
     : (dishes : Set Recipe)
    -> (menu : Dict (Day, Meal) (Maybe (String, Nat)))
    -> MealPlan calories

-- return total number of servings for the plan
export total
totalServings : MealPlan cals -> Nat
totalServings (MkPlan dishes _) = sum (map servings (Set.toList dishes))

-- return the number of servings that have been assigned to a menu
export total
assignedServings : MealPlan cals -> Nat
assignedServings (MkPlan _ menu) = foldl addServing 0 menu
  where
    addServing : Nat -> Maybe (String, Nat) -> Nat
    addServing accum Nothing = accum
    addServing accum (Just (_, servings)) = accum + servings

-- return the number of servings not assigned to the menu
-- can't return nat here: we must prove that assignedServings <= totalServings!
-- I'm sure it can be done, but I am not sure how
export total
unassignedServings : MealPlan cals -> Int
unassignedServings plan = (cast (totalServings plan)) - (cast (assignedServings plan))

export total
accumServings : Day -> Dict String Nat -> ((Day, Meal), Maybe (String, Nat)) -> Dict String Nat
accumServings day accum ((d, m), Just (r, s)) =
   if d == day
   then case (lookup r accum) of
     Nothing => accum
     Just servings => insert r (servings + s) accum
   else accum
accumServings day accum ((d, m), Nothing) = accum

-- return the list of assigned servings for a given day in the plan
export total
servingsPerDay : MealPlan cals -> Day -> Dict String Nat
servingsPerDay (MkPlan _ menu) day = foldl (accumServings day) Dict.empty (Dict.toList menu)

{-- user input --}

-- the interaction is based on servings and calories:

-- we start off with an empty plan
export total
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
