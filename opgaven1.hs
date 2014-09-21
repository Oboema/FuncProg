import FPPrac
import Debug.Trace

-- opgave 1
f :: Number -> Number
f x =	2 * x^2 + 3*x - 5


-- opgave 2

--						wrap around with modulo fucks up when you don't normalize 'a' to zero
wrap :: Number -> Char -> Char -> Char -> Char
wrap n c start stop = chr( ord start + mod (ord c - ord start + n) ( ord stop - ord start + 1))

codeer :: Number -> Char -> Char
-- call ' map (codeer 3) "je moeder" ' to apply codeer to a string 
codeer n c 	| 'a' <= c && c <= 'z' = wrap n c 'a' 'z' --chr ( ord 'a' +  mod (ord c - ord 'a' + 3 ) (ord 'z' - ord 'a' ) )
			| 'A' <= c && c <= 'Z' = wrap n c 'A' 'Z'
			| otherwise = c

-- opgave 3
rente :: Number -> Number -> Number -> Number

rente start rate years 	| years == 0 = start
						| otherwise = rente (start * rate) rate (years - 1)


-- opgave 4
discr :: Number -> Number -> Number ->Number
wortel :: Number -> Number -> Number -> String

discr a b c = b^2 - 4*a*c
wortel a b c 	| disc < 0 = error "discriminant negatief    Disc: " ++ show disc
				| disc == 0 = "wortel: " ++ show ( (-b) / (2*a) ) ++  " Disc: " ++ show disc
				| otherwise = "wortel1: " ++ show ( ((-b)+sqrt(disc)) / (2*a) ) ++ "   wortel2: " ++ show ( ((-b)-sqrt(disc)) / (2*a) ) ++ "  Disc: " ++ show disc
					where disc = discr a b c  


-- opave 5
extrX :: Number -> Number -> Number ->Number
extrY :: Number -> Number -> Number ->Number

extrX a b c = (-b)/(2*a)
extrY a b c = a * iks^2 + b * iks + c
	where iks = extrX a b c


-- opgave 6

mylength :: [a] -> Number

mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mysum :: [Number] -> Number
mysum [] = 0
mysum (x:xs) = x + mysum xs

{- Dit is dus niet de bedoeling

myreverse' :: [a] -> [a] -> [a]
myreverse :: [a] -> [a]

myreverse [] 			= []
myreverse (x: xs) 		= myreverse' [x] xs
myreverse' rev (x:src)	= myreverse' (x : rev) src
myreverse' rev []		= rev
-}

myreverse :: [a] -> [a]
myreverse [] 		= []
myreverse (x:xs)	= (myreverse xs) ++ [x]

mytake :: Number -> [a] -> [a]
mytake 0 (x:xs)		= []
mytake n (x:xs) 	= x: (mytake (n-1) xs)

myelem :: Eq a => a -> [a] -> Bool
myelem el []		= False
myelem el (x:xs) 	= el == x 	||  myelem el xs

{-
myconcat :: [[a]] -> [a] 
myconcat []			= []
--myconcat [x] 		= x
myconcat (x :xs) 	= x ++ (myconcat xs)
-}
mymaximum :: Ord a => [a] -> a
mymaximum []		= error "Cannot find maximum of an empty list"
mymaximum [x]		= x
mymaximum (x:xs)	| x > mymaximum xs 	= x
					| otherwise 		= mymaximum xs

-- lijsten van 1 element concatten voelt inefficient
myzip :: [a] -> [b] -> [(a,b)] 
myzip [] _ 		= []
myzip _  []		= []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys


-- Opgave 7
r :: Number -> Number -> [Number]
r start offset = start : r (start+offset) offset

r1 :: Number -> [Number] -> Number

r1 n xs = last (mytake n xs)

totaal :: Number -> Number ->  [Number] -> Number
totaal i j xs =  mysum (drop i (take (j+1) xs) )

-- Opgave 8

allEqual :: Eq a => [a] -> Bool
allEqual []		= True
allEqual [x]	= True
allEqual (x:xs)	= myelem x xs && allEqual xs 

isRR :: [Number] -> Bool
isRR []		= False
isRR [x] 	= False
isRR (x:xs) = mytake (mylength (x:xs)) ( r x ( (head xs) - x) ) == (x : xs)

mRowsEqual :: [[Number]] -> Bool
mRowsEqual []			= True
mRowsEqual (xs:[]) 		= True
mRowsEqual (xs:xss)		= (mylength xs == mylength (head xss)) && mRowsEqual ( xss)
	--"len xs = "++show (mylength xs) ++", len head xss = "++show (mylength (head xss)) ++ ", tail: " ++ show (tail xss) ++ ", rest: " ++ mRowsEqual (tail xss)
	--

mRowSums :: [[Number]] -> [Number]
mRowSums []			= error "No sums of the empty list"
mRowSums (xs:[])	= [mysum xs] --"mysum xs = "++show (mysum xs)++" SLC "
mRowSums (xs:yss)   = [mysum xs] ++ mRowSums yss --"mysum xs = "++show (mysum xs)++" : " ++ mRowSums yss

{-}
mTranspose :: [[Number]] -> [[Number]]
mTranspose xss | trace (" mTranspose " ++ show xss) False = undefined 
mTranspose [] 				= []
mTranspose ([] : xss)		= mTranspose xss
mTranspose ((x:xs) : xss)	= (x : [h | (h:_) <- xss]) : mTranspose (xs : [ t | (_:t) <- xss])
-}
mTp2 :: [[Number]] -> [[Number]]

mTp2 []			= []
mTp2 ([]:xss)	= mTp2 xss
mTp2 (xss)		= map head xss : mTp2  (map tail xss) -- (trace (" map tail xss: " ++ show (map tail xss))  (mTp2  (map tail xss))  )

mColSums :: [[Number]] -> [Number]
mColSums  	= mRowSums . mTp2  