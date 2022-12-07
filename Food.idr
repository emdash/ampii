module Food


-- Map food name to preferred source
export total
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

