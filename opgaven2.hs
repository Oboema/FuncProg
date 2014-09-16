import FPPrac
import Debug.Trace


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
	deriving Show

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

