import FPPrac
import Debug.Trace
import Data.Char
import qualified Data.List -- only using this for sortBy, gives namespace errors with fpprac for many other useful functions


--opgave 1
a = [1..10]
rand = [1,5,8,09,6,34,2,43,5,7,6,4,2,2,456,788,53,2,234,2,6,8]

myfilter :: ( a -> Bool ) -> [a] -> [a]
myfilter f xs   = [ k | k <- xs, f k]


--myfoldl :: ( a -> a -> a ) -> a -> [a] -> a
myfoldl f z []      = z
myfoldl f z (x:xs)  = myfoldl f (f z x) xs

--myfoldr :: (a -> b -> a ) -> a -> [b] -> a
myfoldr :: (a -> a -> a ) -> a -> [a] -> a
myfoldr f z []      = z
myfoldr f z (x:xs)  = f x (myfoldr f z xs)

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f xs []           = []
myzipWith f [] ys           = []
myzipWith f (x:xs) (y:ys)   = (f x y) : myzipWith f xs ys

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
krijgNaam       :: Persoon -> String
krijgNaam       (naam, _, _, _)     = naam
krijgLeeftijd   :: Persoon -> Number
krijgLeeftijd   (_,leeftijd,_,_)    = leeftijd
krijgGeslacht   :: Persoon -> Gender
krijgGeslacht   (_,_,geslacht,_)    = geslacht
krijgStad       :: Persoon -> String
krijgStad       (_,_,_,stad)        = stad

--2c
incLeeftijdRec  :: Number -> [ Persoon ] -> [Persoon]
incLeeftijdRec inc []               = []
incLeeftijdRec inc ((n,l,g,s):xs)   = (n,l+inc,g,s) : (incLeeftijdRec inc xs)

incLeeftijdLijst    :: Number -> [Persoon] -> [Persoon]
incLeeftijdLijst inc [] = []
incLeeftijdLijst inc xs = [(n,l+inc,g,s) | (n,l,g,s) <- xs ]

incLeeftijdTuple :: Number -> Persoon -> Persoon
incLeeftijdTuple inc (n,l,g,s)  = (n,l+inc,g,s)

incLeeftijdHigh :: Number -> [Persoon] -> [Persoon]
incLeeftijdHigh inc []  = []
incLeeftijdHigh inc xs  = map (incLeeftijdTuple inc ) xs

--2d


getMilfRec      :: [ Persoon ] -> [Persoon]
getMilfRec  []              =   []
getMilfRec  ((n,l,g,s):xs)  | g == Vrouw && 30 < l && l < 40    = (n,l,g,s) : (getMilfRec xs)
                            | otherwise     = getMilfRec xs 


getMilfLijst    :: [ Persoon ] -> [Persoon]
getMilfLijst    []  = []
getMilfLijst    xs  = [(n,l,g,s) | (n,l,g,s) <- xs, g == Vrouw && 30 < l && l < 40 ]

isMilf          :: Persoon -> Bool
isMilf  (n,l,g,s)   = g == Vrouw && 30 < l && l < 40

getMilfHigh     :: [ Persoon ] -> [Persoon]
getMilfHigh     []  = []
getMilfHigh xs  = myfilter isMilf xs 

--e
getAgeByName :: String -> [Persoon] -> Number
getAgeByName name []    = error "No people in database"
getAgeByName name ((n,l,_,_):xs)    | (map toLower n) == (map toLower name) = l 
                                    | otherwise = getAgeByName name xs

--f 
comperson :: Persoon -> Persoon -> Ordering
comperson (_,l,_,_) (_,l',_,_)  =   compare l l'

sorteerPersonen :: [Persoon ] -> [Persoon]
sorteerPersonen []      = []
sorteerPersonen xs      = Data.List.sortBy comperson xs 

--3.a
geenzeef ::  [Number] -> [Number]

geenzeef (x:xs)     = x : geenzeef ( [ k | k <- xs , (k `mod` x) /= 0])

ispriem :: Number -> Bool
ispriem n = any (n==) (take n $ geenzeef [2..])


priemen :: Number -> [Number]
priemen n   = take n (geenzeef [2..])

kleinerpriemen :: Number -> [Number]
kleinerpriemen n    = takeWhile (< n) (geenzeef [2..])


--3.b eigenlijk priemfac, superinefficient maarja
delers' :: Number -> Number -> [Number]
delers' k 1 = []
delers' k n | (n `mod` k) == 0  = k: delers' k (round $ n/k)
            | otherwise         = delers'   (k+1) n

delers :: Number -> [Number]
delers  n   = delers' 2 n

isPriem :: Number -> Bool
isPriem n = [n] == delers n --  want lazy eval length ( delers n)  == 1 

--4a&b

pyth :: Number -> [(Number, Number, Number)]
--pyth' ::  [(Number,Number,Number)]

pyth'   = [(a,b,c) | c <- [1..], b <-[1..c], a <- [1..b], a^2 + b^2 == c^2]
pyth n  = take n pyth'

-- 5.a
stijgend :: [Number] -> Bool
stijgend []     = True
stijgend [x]    = True
stijgend (x:y:xs)   = (x < y) && stijgend (y:xs)

--5b
zwakStijgend' :: Number -> Number -> [Number] -> Bool
zwakStijgend' gem n []      = True
zwakStijgend' gem n (x:xs)  = (gem < x) &&  ( zwakStijgend' ((gem*(n/(n+1))) + (x/n))  (n+1)  xs )

zwakStijgend :: [Number] -> Bool
zwakStijgend []     = True
zwakStijgend xs     = zwakStijgend' 0 1 xs

--6a

-- length ys moet weg anders kan je niet oneindige lijst vergelijken
sublijst :: Eq a => [a] -> [a] -> Bool
sublijst [] []          = True
sublijst [] ys          = True
sublijst xs []          = False
sublijst (x:xs) (y:ys)  | x == y    = sublijst xs ys
                        | otherwise = sublijst (x:xs) ys    

-- sublijst (5:(drop 50 (take 70 [1..]))) [1..]
-- sublijst ((drop 50 (take 70 [1..]))) [1..]



--6b

deellijst :: Eq a => [a] -> [a] -> Bool
deellijst xs ys     = deellijst' False xs ys

deellijst' :: Eq a => Bool -> [a] -> [a] -> Bool
deellijst' _ [] ys  = True
deellijst' _ xs [] = False
deellijst' matched (x:xs) (y:ys)    | x == y                    = deellijst' True  xs ys
                                    | x /= y && (not matched)   = deellijst' matched (x:xs) ys
                                    | otherwise                 = False
-- deellijst (5:(drop 50 (take 70 [1..]))) [1..]
--  deellijst ((drop 50 (take 70 [1..]))) [1..]

--7
{--}
bubble2 :: Ord a => [a] -> [a] -> [a]
bubble2 sorted (unsorted:[])    = sorted ++ [unsorted]
bubble2 sorted (u:n:unsorted)   | u < n     = bubble2 (sorted ++ [u]) (n : unsorted)
                                | otherwise = bubble2 (sorted ++ [n]) (u : unsorted)


bsort2 :: Ord a => [a] -> [a]
bsort2 xs   | xs == (bubble2 [] xs) = xs
            | otherwise             = bsort2 ( bubble2 [] xs)
            
--8
mmsort ::  Ord a => [a] -> [a]
mmsort []   = []
mmsort [x]  = [x]
mmsort xs   = mi : (mmsort (xs Data.List.\\ [mi,ma])) ++ [ma]
    where
        mi = minimum xs
        ma = maximum xs
--9
isort :: Ord a => [a] -> [a] -> [a]
isort [] ys         = ys
isort (x:xs)  []    = isort xs [x]
isort (x:xs) (y:ys) | x <= y    = isort xs (x:y:ys)
                    | otherwise = isort xs (y: ( isort [x] ys))

--10
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = merge xs (x:y:ys)
                    | otherwise = merge xs (y: (merge [x] ys))

msort :: (Show a, Ord a) => [a] -> [a]

msort []    = []
msort [x]   = [x]
msort xs    = merge (msort(traceShow lhalf lhalf)) (msort(traceShow rhalf rhalf))
    where
        l               = round ((length xs)/2)
        split xs        = [take l xs, drop l xs]
        (lhalf,rhalf)   = (take l xs, drop l xs)

--11
ts :: Show a => [a] -> [a]
ts thing    = traceShow thing thing

t :: Show a => String -> a -> a
t str thing = trace (str ++ (show thing)) thing

qsort :: (Show a, Ord a) => [a] -> [a]
qsort []        = []
--qsort [x]     = [x]
qsort (x:xs)    = (qsort less) ++ [x] ++ (qsort more)
    --(qsort (t "less" less)) ++ [x] ++ (qsort (t "more" more))

    --(qsort less) ++ (qsort equal) ++ (qsort more)
    --(qsort (t "less" less)) ++ (qsort (t "eq" equal)) ++ (qsort (t "more" more))
    where
        less    = filter (<=x)  xs
        --equal = x: ( filter (==x) xs)
        more    = filter (>x)  xs