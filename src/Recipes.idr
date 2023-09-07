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


import Measures
import Food



||| All physical unit abbreviations in a flat enumeration
public export 
data Unit
  -- weight
  = Oz
  | Lb
  | Gram 
  | Gal
  -- volume
  | Qt
  | Pt
  | C
  | Floz
  | Tblsp
  | Tsp
  | ML
  | L
  -- size
  | Small
  | Med
  | Reg
  | Large
  | XLarge
  | Jumbo
  | Whole  -- size unspecified


||| An ingredient of in a recipe, with its quantity and unit
public export 
data Ingredient = Item Double Recipes.Unit String


||| A Recipe is a list of ingredient quantities, plus metadata.
public export
record Recipe where
  constructor Rx
  name: String
  servings: Nat
  ingredients: List Ingredient


||| Recipes are compared by name
export 
Eq Recipe where
  x == y = (name x) == (name y)


||| Recipes are ordered by name
export 
Ord Recipe where
  compare x y = compare (name x) (name y)



||| A hard-coded, static recipe database.
|||
||| Each Recipe is identified by a short string key.
|||
||| Eventually the goal would be to support some standard file format,
||| like meal-master, etc, and reading these off the filesystem. But,
||| at this point I don't know enough about Idris to do that.
|||
||| For now, each are encoded by hand using the DSL defined in this
||| file.
export total
recipes : String -> Recipe
recipes n@"French Toast" = Rx n 1 [
  Item  2.0  Large  "Egg",
  Item  2.0  Whole  "Bread Slice",
  Item  1.0  Tblsp  "Butter",
  Item  1.0  Tblsp  "Maple Syrup"
]
-- https://youtu.be/L76XJqz9PWo
-- XXX: this recipe is incomplete (and open-ended)
recipes n@"Shakshuka" = Rx n 2 [
  Item  2.0  Large "Egg",
  Item 14.5  Oz    "Diced Tomatoes"
]
recipes n@"Toaster Pizza" = Rx n 1 [
  Item  1.0  Med   "Tortilla",
  Item  1.5  Tblsp "Pizza Sauce",
  Item  1.5  Oz    "Mozarella",
  Item  1.5  Oz    "Italian Sausage"
]
-- https://www.thespruceeats.com/stuffed-eggplant-little-shoes-1705821
recipes n@"Greek Stuffed Eggplant" = Rx n 6 [
  Item  1.5  Small "Eggplant",         
  Item  1.5  Lb    "Ground Beef",
  Item  1.5  Med   "Garlic Clove",
  Item  1.5  Med   "Onion",            
  Item  1.5  C     "Parsley",
  Item  1.5  Tblsp "Olive Oil",
  Item  1.5  C     "Grated Parmesian",
  Item  1.5  Med   "Tomato",
  Item  1.5  Tblsp "All Purpose Flour",
  Item  1.5  Tblsp "Butter",
  Item  1.5  C     "Milk",             
  Item  1.5  ML    "Ground Nutmeg"
]
-- https://www.thespruceeats.com/creole-shrimp-etouffee-3060807
recipes n@"Creole Shrimp Etouffee" = Rx n 6 [
  Item  7.0  Tblsp "Butter",                      
  Item  6.0  Tblsp "All Purpose Flour",
  Item  1.0  Large "Onion",                       
  Item  1.5  C     "Celery",                      
  Item  1.0  C     "Green Bell Pepper",
  Item  3.0  Large "Garlic Clove",                
  Item  8.0  Floz  "Shrimp Stock / Clam Juice",
  Item 14.5  Floz  "Diced Tomatoes",              
  Item  2.0  Tblsp "Creole Seasoning (Salt Free)",
  Item  0.2  Tsp   "Black Pepper",
  Item  1.0  Large "Bay Leaf",                    
  Item  1.5  Lb    "Shrimp (Peeled and Deveined)",
  Item  2.0  C     "Cooked Rice"
]
-- https://cooking.nytimes.com/recipes/1021339-ramen-with-charred-scallions-green-beans-and-chile-oil
recipes n@"Ramen with Scallions" = Rx n 4 [
  Item  2.0  Tblsp "Red Pepper Flakes",
  Item  1.5  Tsp   "Kosher Salt",
  Item  0.5  C     "Grapeseed Oil",
  Item  0.5  Whole "Ginger Root",               
  Item  2.0  Whole "Garlic Clove",
  Item  2.0  Tsp   "Sesame Seeds",
  Item  1.0  Tsp   "Sesame Oil",
  Item  0.0  ML    "Kosher Salt",
  Item 12.0  Oz    "Ramen Noodles",
  Item 12.0  Whole "Scallion",               
  Item  3.0  Tblsp "Grapeseed Oil",
  Item 10.0  Oz    "Green Beans",
  Item  0.5  Whole "Ginger Root",
  Item  0.0  Gram  "White Pepper",
  Item  1.0  Tblsp "Sesame Seeds"
]
-- https://cooking.nytimes.com/recipes/1022479-sheet-pan-gnocchi-with-mushrooms-and-spinach
recipes n@"Sheet Pan Gnocci" = Rx n 4 [
  Item  1.0  Lb    "Mixed Mushrooms",
  Item  6.0  Tblsp "Olive Oil",
  Item  4.0  Whole "Scallion",
  Item  1.0  Whole "Large Shallot",
  Item  5.0  Oz    "Baby Spinach",
  Item  2.0  Tblsp "Dijon Mustartd",
  Item  2.0  Tblsp "Horseradish",
  Item  1.0  Tsp   "Honey",
  Item  1.0  Tblsp "Butter (Unsalted)"
]
-- https://cooking.nytimes.com/recipes/1021842-jamaican-curry-chicken-and-potatoes
recipes n@"Jamaican Curry Chicken" = Rx n 4 [
  Item  3.0  Lb    "Chicken Thighs",
  Item  4.0  Tblsp "Garlic Powder",
  Item  2.0  Tsp   "Kosher Salt",
  Item  2.0  Tblsp "Olive Oil",
  Item  1.0  Whole "Large Onion",
  Item  4.0  Whole "Garlic Clove",
  Item  2.0  Tblsp "Jamaican Curry Powder",
  Item  1.0  Whole "Scotch Bonnet",              
  Item  4.0  Med   "Yukon Gold Potato",
  Item  2.0  Qt    "Chicken Stock",              
  Item  1.0  Whole "Bay Leaf",                   
  Item  2.0  Whole "Fresh Time Sprig",
  Item  1.0  C     "Cornstarch",
  Item  2.0  C     "Cooked White Rice"
]
-- https://cooking.nytimes.com/recipes/1021066-chile-crisp-shrimp-and-green-beans
recipes n@"Chile-Crisp Shrimp" = Rx n 4 [
  Item  1.0  Tblsp "Low Sodium Soy Sauce",
  Item  1.0  Tsp   "Granulated Sugar",
  Item  1.0  Tsp   "Red Pepper Flakes",
  Item  0.7  Tsp   "Ground Cumin",
  Item  1.0  Lb    "Shrimp (Peeled and Deveined)",
  Item  1.0  Tsp   "Kosher Salt",
  Item  1.0  Tsp   "Black Pepper",
  Item  4.0  Whole "Garlic Clove",
  Item  1.0  Whole "Cinnamon Stick",
  Item 10.0  Oz    "Green Beans",
  Item  0.2  C     "Roasted, Salted Peanuts"     
]
-- https://cooking.nytimes.com/recipes/8135-roasted-cod-and-potatoes
recipes n@"Roasted Cod and Potatoes" = Rx n 4 [
  Item  5.0  Med   "Medium Potatoes",
  Item  5.0  Tblsp "Butter",
  Item  5.0  Lb    "Cod Fillets"
]
-- https://www.pbs.org/food/recipes/mapo-tofu/
recipes n@"Mapo Tofu" = Rx n 3 [
  Item  0.5  C     "Low Sodium Chicken Broth",
  Item  2.0  Tsp   "Corn Starch",             
  Item  1.0  Tsp   "Sugar",                   
  Item  1.0  Tblsp "Sesame Oil",              
  Item  2.0  Whole "Garlic Clove",            
  Item  4.0  Whole "Scallion",                
  Item  1.0  Tblsp "Black Bean Paste",        
  Item  0.5  Tsp   "Sichuan Peppercorn",      
  Item  6.0  Oz    "Ground Pork",             
  Item  2.0  Tsp   "Doubanjiang",             
  Item  4.0  Oz    "Silken Tofu"             
]
recipes _ = Rx "Unknown" 0 []
