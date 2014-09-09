double x = x + x

doubleUs x y = double x + double y

doubleSmall x = if x > 100
				then x
				else x+x

guard x | x  <= 10    	= "smaller than 10"
		| x  <= 20 		= "between 10 and 20"
		| otherwise		= "over 20"

guard' x
		| x  <= 10    	= "smaller than 10"
		| x  <= 20 		= "between 10 and 20"
		| otherwise		= "over 20"