doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleUs2 x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
			then x
			else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1  

conanO'Brien  = "hello"

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!"| x <- xs, odd x]

removeUppercases xs = [x | x <- xs, not (x `elem` ['A'..'Z'])]

removeUppercases xs = [x | x <- xs, not (x `elem` ['A'..'Z'])]
