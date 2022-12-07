module Shopping


import Data.AVL.Dict
import Recipes
import Measures


total
sumAmounts
  :  Dict String Quantity
  -> Ingredient
  -> Dict String Quantity
sumAmounts accum (I food amount) = case lookup food accum of
  Nothing => insert food amount                 accum
  Just a  => insert food (addQuantity a amount) accum

-- for some reason foldl with sumAmounts loops forever, and I had to
-- write this fold by hand.  maybe I should be try foldr? do not
-- know.
total
foldRecipes : List Ingredient -> Dict String Quantity -> Dict String Quantity
foldRecipes []        ret = ret
foldRecipes (x :: xs) ret = (sumAmounts (foldRecipes xs ret) x)

export total
shoppingList : List Recipe -> Dict String Quantity
shoppingList        [] = Dict.empty
shoppingList (r :: rs) = foldRecipes (ingredients r) (shoppingList rs)
