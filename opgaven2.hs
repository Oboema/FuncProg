import FPPrac
import Debug.Trace
import Data.Char
import qualified Data.List -- only using this for sortBy, gives namespace errors with fpprac for many other useful functions


--opgave 1
a = [1..10]

myfilter :: ( a -> Bool ) -> [a] -> [a]
myfilter f xs	= [ k | k <- xs, f k]


myfoldl :: ( a -> a -> a ) -> a -> [a] -> a
myfoldl f z []		= z
myfoldl f z (x:xs)	= myfoldl f (f z x) xs

--myfoldr :: (a -> b -> a ) -> a -> [b] -> a
myfoldr :: (a -> a -> a ) -> a -> [a] -> a
myfoldr f z []		= z
myfoldr f z (x:xs)	= f x (myfoldr f z xs)

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f xs []			= []
myzipWith f [] ys			= []
myzipWith f (x:xs) (y:ys)	= (f x y) : myzipWith f xs ys

--opgave 2
data Gender = Man | Vrouw | Schmuz
	deriving (Show, Eq)

--2a : type Persoon: ( String , Number, Gender, String)

type Persoon = ( String , Number, Gender, String)
	--deriving Show

db = [("Jaap",32,Man,"Enske"),("Jemoeder", 99, Vrouw, "Red light district"),
	("Travo",43,Schmuz,"Utrecht"),("Jannie",38,Vrouw,"Den Haag"),
	("Gabi",32,Vrouw,"Amsterdam"),("Kees de Boer", 26,Man,"Minroe")]
-- 2b
krijgNaam 		:: Persoon -> String
krijgNaam 		(naam, _, _, _)		= naam
krijgLeeftijd 	:: Persoon -> Number
krijgLeeftijd 	(_,leeftijd,_,_)	= leeftijd
krijgGeslacht 	:: Persoon -> Gender
krijgGeslacht 	(_,_,geslacht,_)	= geslacht
krijgStad 		:: Persoon -> String
krijgStad		(_,_,_,stad)		= stad

--2c
incLeeftijdRec	:: Number -> [ Persoon ] -> [Persoon]
incLeeftijdRec inc []				= []
incLeeftijdRec inc ((n,l,g,s):xs)	= (n,l+inc,g,s) : (incLeeftijdRec inc xs)

incLeeftijdLijst	:: Number -> [Persoon] -> [Persoon]
incLeeftijdLijst inc []	= []
incLeeftijdLijst inc xs	= [(n,l+inc,g,s) | (n,l,g,s) <- xs ]

incLeeftijdTuple :: Number -> Persoon -> Persoon
incLeeftijdTuple inc (n,l,g,s)	= (n,l+inc,g,s)

incLeeftijdHigh	:: Number -> [Persoon] -> [Persoon]
incLeeftijdHigh inc []	= []
incLeeftijdHigh inc xs	= map (incLeeftijdTuple inc ) xs

--2d


getMilfRec		:: [ Persoon ] -> [Persoon]
getMilfRec	[]				=	[]
getMilfRec	((n,l,g,s):xs) 	| g == Vrouw && 30 < l && l < 40	= (n,l,g,s) : (getMilfRec xs)
							| otherwise		= getMilfRec xs 


getMilfLijst	:: [ Persoon ] -> [Persoon]
getMilfLijst	[]	= []
getMilfLijst	xs	= [(n,l,g,s) | (n,l,g,s) <- xs, g == Vrouw && 30 < l && l < 40 ]

isMilf			:: Persoon -> Bool
isMilf	(n,l,g,s)	= g == Vrouw && 30 < l && l < 40

getMilfHigh 	:: [ Persoon ] -> [Persoon]
getMilfHigh 	[]	= []
getMilfHigh	xs	= myfilter isMilf xs 

--e
getAgeByName :: String -> [Persoon] -> Number
getAgeByName name []	= error "No people in database"
getAgeByName name ((n,l,_,_):xs)	| (map toLower n) == (map toLower name) = l 
									| otherwise = getAgeByName name xs

--f 
comperson :: Persoon -> Persoon -> Ordering
comperson (_,l,_,_) (_,l',_,_)	=	compare l l'

sorteerPersonen :: [Persoon ] -> [Persoon]
sorteerPersonen []		= []
sorteerPersonen xs		= Data.List.sortBy comperson xs 

--3.a
--zeef :: [Number] -> [Number]
--zeef (x:xs)	= x 

--3.b eigenlijk priemfac, superinefficient maarja
delers' :: Number -> Number -> [Number]
delers' k 1 = []
delers' k n | (n `mod` k) == 0 	= k: delers' k (round $ n/k)
			| otherwise 		= delers'	(k+1) n

delers :: Number -> [Number]
delers	n 	= delers' 2 n

isPriem :: Number -> Bool
isPriem n = length ( delers n)  == 1 

--4a&b

pyth :: Number -> [(Number, Number, Number)]
--pyth' ::  [(Number,Number,Number)]

pyth'	= [(a,b,c) | c <- [1..], b <-[1..c], a <- [1..b], a^2 + b^2 == c^2]
pyth n 	= take n pyth'

-- 5.a
stijgend :: [Number] -> Bool
stijgend [] 	= True
stijgend [x]	= True
stijgend (x:xs)	= (x < (head xs)) && stijgend xs

--5b
zwakStijgend' :: Number -> Number -> [Number] -> Bool
zwakStijgend' gem n []		= True
zwakStijgend' gem n (x:xs)	= (gem < x) &&  ( zwakStijgend' ((gem*(n/(n+1))) + (x/n))  (n+1)  xs )

zwakStijgend :: [Number] -> Bool
zwakStijgend [] 	= True
zwakStijgend xs		= zwakStijgend' 0 1 xs

--6a

 
sublijst :: Eq a => [a] -> [a] -> Bool
--sublijst [] []		= True
--sublijst xs []		= False
--sublijst [] ys		= True
sublijst xs ys		| length ys >= length xs	= (take (length xs) ys == xs ) || sublijst xs (tail ys)
					| otherwise					= False

--6b
deellijst :: Eq a => [a] -> [a] -> Bool
deellijst [] ys	= True
deellijst xs [] = False
deellijst (x:xs) ys | x == (head ys) 	= deellijst xs (tail ys)
					| otherwise			= deellijst (x:xs) (tail ys)
--7
{--}
bubble2 :: Ord a => [a] -> [a] -> [a]
bubble2 sorted (unsorted:[])	= sorted ++ [unsorted]
bubble2 sorted (u:n:unsorted)	| u < n 	= bubble2 (sorted ++ [u]) (n : unsorted)
								| otherwise = bubble2 (sorted ++ [n]) (u : unsorted)


bsort2 :: Ord a => [a] -> [a]
bsort2 xs	| xs == (bubble2 [] xs)	= xs
			| otherwise				= bsort2 ( bubble2 [] xs)
			

