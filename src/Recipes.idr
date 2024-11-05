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


||| A DSL and Operations on Recipes
module Recipes


import Derive.Prelude
import Measures
import Food


%default total
%language ElabReflection


||| An entry in a recipe's ingredient list.
|||
||| I find it interesting that the most natural form of these
||| constructors are verbs.
|||
||| I tried making `Ingredient` a dependent record indexed over
||| dimension, but Idris doesn't quite like this. So, instead, I made
||| `Ingredient` a sum type, with explicit constructors for each
||| supported Dimension.
|||
||| Originally, I used passive constructors: `ByWeight`, `ByVolume`,
||| `ByEach`. But I felt it looked odd, and I felt them tedious to
||| write. So I 'verbed' them, and I like it better: before I 'verbed'
||| the constructors, I thought of Ingredients as data; after
||| 'verbing', I realized: an ingredient is *also* an *instruction* to
||| the chef.
|||
||| Before 'verbing': an ingredient quantified by density was legal;
||| after 'verbing': it's impossible to quantify an ingredient by
||| density. In case it isn't: an ingredient is an *amount* of
||| something. It is a quantiy given by *mass*. In other words, an
||| ingredient cannot be a *ratio* of *mass* to *volume*. It must be
||| *either* a mass *or* a volume.
public export
data Ingredient
  = Weigh   (Quantity Mass)   food
  | Measure (Quantity Volume) food
  | Take    (Quantity Scalar) food

||| A Recipe is a list of ingredient quantities, plus metadata.
|||
||| An *ingredient* represents an amount of something, while the
||| recipe as a whole expresses the *ratios between ingredients*.
|||
||| The best historical recipes directly express the proportions
||| between ingredients, rather than the concrete units of the day.
|||
||| TBD: add the following fields;
|||   url:          String
|||   description:  String
|||   image:        String
|||   author:       String
|||   instructions: List Instruction
public export
record Recipe (food: Type) where
  constructor Rx
  name:         food
  yield:        Quantity Mass
  ingredients:  List Ingredient

||| A hard-coded, static recipe database for testing.
|||
||| The yields given here are totaly bogus and made up, so keep that
||| in mind. This is probably bigger than it needs to be, but I can
||| always pare it down later.
|||
||| Originally I had figured I would manually encode recipes, but I
||| already have too many recipes for that.
|||
||| This intended to be test data, keep it in sync with the official
||| file format.
export total
recipes : String -> Recipe String
recipes n@"French Toast" = Rx n 250.g [
  Take     2.0.whole "Large Egg",
  Take     2.0.whole "Bread Slice",
  Measure  1.0.T     "Butter",
  Measure  1.0.T     "Maple Syrup"
]
-- https://youtu.be/L76XJqz9PWo
-- XXX: this recipe is incomplete (and open-ended)
recipes n@"Shakshuka" = Rx n 250.g [
  Take     2.0.whole "Large Egg",
  Weigh   14.5.oz    "Diced Tomatoes"
]
recipes n@"Toaster Pizza" = Rx n 250.g [
  Take     2.0.whole "Medium Tortilla",
  Measure  1.5.T     "Pizza Sauce",
  Weigh    1.5.oz    "Mozarella",
  Weigh    1.5.oz    "Italian Sausage"
]
-- https://www.thespruceeats.com/stuffed-eggplant-little-shoes-1705821
recipes n@"Greek Stuffed Eggplant" = Rx n 500.g [
  Take     1.5.whole  "Small Eggplant",
  Weigh    1.5.lb     "Ground Beef",
  Take     1.5.whole  "Garlic Clove",
  Take     1.5.whole  "Medium Onion",
  Measure  1.5.C      "Parsley",
  Measure  1.5.T      "Olive Oil",
  Measure  1.5.C      "Grated Parmesian",
  Take     1.5.whole  "Tomato",
  Measure  1.5.T      "All Purpose Flour",
  Measure  1.5.T      "Butter",
  Measure  1.5.C      "Milk",
  Measure  1.5.mL     "Ground Nutmeg"
]
-- https://www.thespruceeats.com/creole-shrimp-etouffee-3060807
recipes n@"Creole Shrimp Etouffee" = Rx n 1.Kg [
  Measure  7.0.T     "Butter",
  Measure  6.0.T     "All Purpose Flour",
  Take     1.0.whole "Large Onion",
  Measure  1.5.C     "Celery",
  Measure  1.0.C     "Green Bell Pepper",
  Take     3.0.whole "Large Garlic Clove",
  Measure  8.0.floz  "Shrimp Stock / Clam Juice",
  Measure 14.5.floz  "Diced Tomatoes",
  Measure  2.0.T     "Creole Seasoning (Salt Free)",
  Measure  0.2.t     "Black Pepper",
  Take     1.0.whole "Large Bay Leaf",
  Weigh    1.5.lb    "Shrimp (Peeled and Deveined)",
  Measure  2.0.C     "Cooked Rice"
]
-- https://cooking.nytimes.com/recipes/1021339-ramen-with-charred-scallions-green-beans-and-chile-oil
recipes n@"Ramen with Scallions" = Rx n 500.g [
  Measure  2.0.T     "Red Pepper Flakes",
  Measure  1.5.t     "Kosher Salt",
  Measure  0.5.C     "Grapeseed Oil",
  Take     0.5.whole "Ginger Root",
  Take     2.0.whole "Garlic Clove",
  Measure  2.0.t     "Sesame Seeds",
  Measure  1.0.t     "Sesame Oil",
  Measure  0.0.mL    "Kosher Salt",
  Weigh   12.0.oz    "Ramen Noodles",
  Take    12.0.whole "Scallion",
  Measure  3.0.T     "Grapeseed Oil",
  Weigh   10.0.oz    "Green Beans",
  Take     0.5.whole "Ginger Root",
  Weigh    0.0.g     "White Pepper",
  Measure  1.0.T     "Sesame Seeds"
]
-- https://cooking.nytimes.com/recipes/1022479-sheet-pan-gnocchi-with-mushrooms-and-spinach
recipes n@"Sheet Pan Gnocci" = Rx n 500.g [
  Weigh    1.0.lb    "Mixed Mushrooms",
  Measure  6.0.T     "Olive Oil",
  Take     4.0.whole "Scallion",
  Take     1.0.whole "Large Shallot",
  Weigh    5.0.oz    "Baby Spinach",
  Measure  2.0.T     "Dijon Mustartd",
  Measure  2.0.T     "Horseradish",
  Measure  1.0.t     "Honey",
  Measure  1.0.T     "Butter (Unsalted)"
]
-- https://cooking.nytimes.com/recipes/1021842-jamaican-curry-chicken-and-potatoes
recipes n@"Jamaican Curry Chicken" = Rx n 2.lb [
  Weigh    3.0.lb     "Chicken Thighs",
  Measure  4.0.T      "Garlic Powder",
  Measure  2.0.t      "Kosher Salt",
  Measure  2.0.T      "Olive Oil",
  Take     1.0.whole  "Large Onion",
  Take     4.0.whole  "Garlic Clove",
  Measure  2.0.T      "Jamaican Curry Powder",
  Take     1.0.whole  "Scotch Bonnet",
  Take     4.0.whole  "Medium Yukon Gold Potato",
  Measure  2.0.qt     "Chicken Stock",
  Take     1.0.whole  "Bay Leaf",
  Take     2.0.whole  "Fresh Time Sprig",
  Measure  1.0.C      "Cornstarch",
  Measure  2.0.C      "Cooked White Rice"
]
-- https://cooking.nytimes.com/recipes/1021066-chile-crisp-shrimp-and-green-beans
recipes n@"Chile-Crisp Shrimp" = Rx n 500.g [
  Measure  1.0.T     "Low Sodium Soy Sauce",
  Measure  1.0.t     "Granulated Sugar",
  Measure  1.0.t     "Red Pepper Flakes",
  Measure  0.7.t     "Ground Cumin",
  Weigh    1.0.lb    "Shrimp (Peeled and Deveined)",
  Measure  1.0.t     "Kosher Salt",
  Measure  1.0.t     "Black Pepper",
  Take     4.0.whole "Garlic Clove",
  Take     1.0.whole "Cinnamon Stick",
  Weigh   10.0.oz    "Green Beans",
  Measure  0.2.C     "Roasted, Salted Peanuts"
]
-- https://cooking.nytimes.com/recipes/8135-roasted-cod-and-potatoes
recipes n@"Roasted Cod and Potatoes" = Rx n 3.0.lb [
  Take     5.0.whole  "Medium Potatoes",
  Measure  5.0.T      "Butter",
  Weigh    5.0.lb     "Cod Fillets"
]

-- https://www.pbs.org/food/recipes/mapo-tofu/
recipes n@"Mapo Tofu" = Rx n 1.0.lb [
  Measure  0.5.C     "Low Sodium Chicken Broth",
  Measure  2.0.t     "Corn Starch",
  Measure  1.0.t     "Sugar",
  Measure  1.0.T     "Sesame Oil",
  Take     2.0.whole "Garlic Clove",
  Take     4.0.whole "Scallion",
  Measure  1.0.T     "Black Bean Paste",
  Measure  0.5.t     "Sichuan Peppercorn",
  Weigh    6.0.oz    "Ground Pork",
  Measure  2.0.t     "Doubanjiang",
  Weigh   14.0.oz    "Silken Tofu"
]

recipes _ = Rx "Unknown" 0.g []
