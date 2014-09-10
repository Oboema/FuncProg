import FPPrac

f :: Number -> Number
f x =	2 * x^2 + 3*x - 5


--						wrap around with modulo fucks up when you don't normalize 'a' to zero
wrap :: Number -> Char -> Char -> Char -> Char
wrap n c start stop = chr( ord start + mod (ord c - ord start + n) ( ord stop - ord start + 1))

codeer :: Number -> Char -> Char
-- call ' map (codeer 3) "je moeder" ' to apply codeer to a string 
codeer n c 	| 'a' <= c && c <= 'z' = wrap n c 'a' 'z' --chr ( ord 'a' +  mod (ord c - ord 'a' + 3 ) (ord 'z' - ord 'a' ) )
			| 'A' <= c && c <= 'Z' = wrap n c 'A' 'Z'
			| otherwise = c

rente :: Number -> Number -> Number -> Number

rente start rate years 	| years == 0 = start
						| otherwise = rente (start * rate) rate (years - 1)


wortel1 :: Number -> Number -> Number -> Number

wortel 1