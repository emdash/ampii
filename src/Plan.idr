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

||| Represents a meal plan and operations on it.
module Plan


import Data.Nat
import Data.HashMap
import Recipes
import Util


%default total


||| An opaque date type.
|||
||| Internally, represented as a Julian ordinal.
|||
||| XXX: find a good datetime library for Idris
export 
data Date = Ordinal Nat

||| Equality is defined on Date, so it may be used as a Set key
export 
Eq Date where
  (==) (Ordinal x) (Ordinal y) = x == y

||| Ordering is defined on Date, so it may be used as a Set key
export 
Ord Date where
  compare (Ordinal x) (Ordinal y) = compare x y


||| A Meal type
|||
||| XXX: This type hard-codes cultural assumptions. For now,
||| hard-coding the variants simplifies keeping everything total, but
||| it should eventually be generalized.
public export 
data Meal
  = Breakfast
  | Lunch
  | Dinner
  | Other String

||| Equality is defined on Meal, so it may be used as a Set key.
export 
Eq Meal where
  Breakfast == Breakfast = True
  Lunch     == Lunch     = True
  Dinner    == Dinner    = True
  (Other x) == (Other y) = x == y
  _         == _         = False

||| Ordering is defined on Meal, so it may be used as a Set key.
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


||| The user-specified number of meals usually eaten daily.
|||
||| XXX: these are hard-coded personal preferences
dailyMeals : List Meal
dailyMeals = [Breakfast, Lunch, Dinner]


||| A meal slot is a tuple of (Date, Meal)
export 
MealSlot : Type
MealSlot = (Date, Meal)


||| A Portion is a recipe and a natural number
export 
Portion : Type
Portion = (String, Nat)


||| A nutrient category for use in nutritional goals
public export 
data Nutrient
  = Calories
  | Carbs
  | Fats
  | Protein
  | Fiber

||| Equality is defined for nutrients, so that it may be used with Dict.
export 
Eq Nutrient where
  Calories == Calories = True
  Carbs    == Carbs    = True
  Fats     == Fats     = True
  Protein  == Protein  = True
  Fiber    == Fiber    = True
  _        == _        = False

||| Ordering is defined for nutrients, so that it may be used with Dict.
-- XXX: this ordering is arbitrary, but required for using with Set so
-- we don't export it. Is there a way to derive equality in this
-- situation?
Ord Nutrient where
  compare Calories Calories = EQ
  compare Calories _        = LT
  compare Carbs    Calories = GT
  compare Carbs    Carbs    = EQ
  compare Carbs    _        = LT
  compare Fats     Calories = GT
  compare Fats     Carbs    = GT
  compare Fats     Fats     = EQ
  compare Fats     _        = LT
  compare Protein  Calories = GT
  compare Protein  Carbs    = GT
  compare Protein  Fats     = GT
  compare Protein  Protein  = EQ
  compare Protein  _        = LT
  compare Fiber    Calories = LT
  compare Fiber    Carbs    = LT
  compare Fiber    Fats     = LT
  compare Fiber    Protein  = LT
  compare Fiber    Fiber    = EQ


||| Represents user-specified nutritional goals
export
Nutrition : Type
Nutrition = HashMap Nutrient Double


||| A type for planning what to eat and when.
export 
record MealPlan where
  constructor Plan
  ||| A mapping of per-day nutrition targets for planning purposes.
  dailyGoals  : Nutrition
  ||| The set of recipes mapped number of servings it yields.
  portions    : HashMap String Nat
  ||| Maps from meal slots slots to assigned portions
  menu : HashMap MealSlot (HashMap String Nat)


||| A new, empty meal plan
export
empty : MealPlan
{- empty = Plan empty empty empty -}


||| Add a recipe to a meal plan
|||
||| Servings represents how many total servings the recipe is expected
||| to yield for this plan.
export
addRecipe : String -> Nat -> MealPlan -> MealPlan
addRecipe recipe servings = { portions $= (insert recipe servings) }


||| Add or clear a meal slot to a meal plan
export
addMealSlot : MealSlot -> MealPlan -> MealPlan
addMealSlot slot = { menu $= (insert slot empty) }


||| The ways in which assigning meals to slots can fail
public 
export data AssignError
  = ZeroPortions
  | NoPortions   String Nat
  | NoSuchSlot   MealSlot
  | NoSuchRecipe String
  | Union        AssignError AssignError


||| Try to assign a portion to a meal slot
|||
||| This can fail if:
||| - the given meal slot isn't part of the plan
||| - the given recipe isn't part of the plan
||| - we've run out of portions for the given recipe
||| - we try to assign zero portions of something to a slot
total
assignPortion
  :  MealSlot
  -> Portion
  -> Either AssignError MealPlan
  -> Either AssignError MealPlan
{-
assignPortion _ _ err@(Left _) = err
assignPortion slot portion @ (recipe, servings) (Right plan @ (Plan _ portions menu)) =
    case lookup slot menu of
    Nothing       => Left (NoSuchSlot slot)
    Just assigned => do
        remaining <- fromMaybe (NoSuchRecipe recipe) (lookup recipe portions)
        case isLTE servings remaining of
          (No contra)   => Left (NoPortions recipe servings)
          (Yes LTEZero) => Left ZeroPortions
          (Yes LTESucc) => Right ({
            portions $= (insert recipe (remaining - servings)),
            menu     $= (insert slot (foldDict recipe servings assigned))
          } plan)
-}

total
testPlan : MealPlan
testPlan
  = addRecipe     "Gruel" 100
  $ addRecipe     "Slop"  100
  $ addMealSlot   (Ordinal 0, Breakfast)
  $ addMealSlot   (Ordinal 0, Lunch)
  $ addMealSlot   (Ordinal 0, Dinner)
  $ addMealSlot   (Ordinal 1, Breakfast)
  $ addMealSlot   (Ordinal 1, Lunch)
  $ addMealSlot   (Ordinal 1, Dinner)
  $ empty


total
testPlan2 : Either AssignError MealPlan
testPlan2
  = assignPortion (Ordinal 0, Breakfast) ("Gruel", 10)
  $ assignPortion (Ordinal 0, Lunch)     ("Slop",   5)
  $ assignPortion (Ordinal 0, Dinner)    ("Slop",   5)
  $ assignPortion (Ordinal 1, Breakfast) ("Gruel", 10)
  $ assignPortion (Ordinal 1, Lunch)     ("Slop",   5)
  $ assignPortion (Ordinal 1, Dinner)    ("Slop",   5)
  $ Right testPlan


{-


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
-}
