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

||| This implements the recipe database.
||| Each recipe is identified by a unique string title.
module Recipes


import Measures

||| A recipe ingredient is a tuple of food and quantity
public export data Ingredient = I String Quantity

||| A Recipe is a list of ingredients, plus some metadata for planning.
public export
record Recipe where
  constructor Rx
  name: String
  servings: Nat
  ingredients: List Ingredient

export Eq Recipe where
  x == y = (name x) == (name y)

export Ord Recipe where
  compare x y = compare (name x) (name y)


{- Global Recipes Database -}


export total
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
