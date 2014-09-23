import FPPrac
import Debug.Trace
import Data.Char
import qualified Data.List -- only using this for sortBy, gives namespace errors with fpprac for many other useful functions
import RoseTree

import qualified Prelude

ts :: Show a => [a] -> [a]
ts thing	= traceShow thing thing

t :: Show a => String -> a -> a
t str thing	= trace (str ++ (show thing)) thing

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a
	deriving Show

t1a = Node1a 7 
		(Node1a 4
			(Node1a 91
				(Leaf1a 37)
				(Leaf1a 13)
			)
			(Leaf1a 14)

		)
		(Node1a 8
			(Node1a 44
				(Leaf1a 4)
				(Node1a 20
					(Node1a 55
						(Leaf1a 44)
						(Leaf1a 21)
					)
					(Leaf1a 17)
				)
			)
			(Node1a 39
				(Leaf1a 18)
				(Node1a 29
					(Leaf1a 9)
					(Leaf1a 12)
				)
			)
		)

--1a
pp1a :: Tree1a -> RoseTree

pp1a (Leaf1a x)			= RoseNode (show x) []
pp1a (Node1a x t1 t2)	= RoseNode (show x) [pp1a t1, pp1a t2]

--1b
data Tree1b = Leaf1b (Number, Number) | Node1b (Number,Number) Tree1b Tree1b
	deriving Show

t1b = Node1b (7,3) 
		(Leaf1b (5,4))
		(Node1b (99,8)
			(Node1b (55,44)
				(Leaf1b (4,4) )
				(Leaf1b (455,20) )
			)
			(Leaf1b (22,39))
		)


pp1b :: Tree1b -> RoseTree

pp1b (Leaf1b x)			= RoseNode (show x) []
pp1b (Node1b x t1 t2)	= RoseNode (show x) [pp1b t1, pp1b t2]

--1c
data Tree1c = Leaf1c | Node1c Int Tree1c Tree1c
	deriving Show

t1c = Node1c 7 
		(Leaf1c )
		(Node1c 8
			(Node1c 44
				(Leaf1c )
				(Leaf1c )
			)
			(Leaf1c )
		)

pp1c :: Tree1c -> RoseTree

pp1c (Leaf1c )			= RoseNode "" []
pp1c (Node1c x t1 t2)	= RoseNode (show x) [pp1c t1, pp1c t2]

--1d
data Tree1d = Leaf1d (Number, Number)| Node1d [Tree1d]
	deriving Show

t1d 	= Node1d [ Node1d [Leaf1d (3,4), Leaf1d (5,9), Node1d [ Leaf1d (66,67), Leaf1d(91,43), Leaf1d (3,3)] ], Leaf1d (5,5), Node1d[ Node1d [ Leaf1d(13,37)]] ]

pp1d :: Tree1d -> RoseTree

pp1d (Leaf1d x) 		= RoseNode (show x) []
pp1d (Node1d xs)		= RoseNode "" [ pp1d x | x <- xs]

--2a
add1a :: Number-> Tree1a -> Tree1a 

add1a k (Leaf1a x) 			= Leaf1a (x+k)
add1a k (Node1a x t1 t2) 	= (Node1a (x+k) (add1a k (t1)) (add1a k (t2)) )

--2b
square1a :: Tree1a -> Tree1a 

square1a (Leaf1a x) 			= Leaf1a (x*x)
square1a (Node1a x t1 t2) 		= (Node1a (x*x) (square1a t1) (square1a t2) )

--2c
mapTree :: (Number -> Number) -> Tree1a -> Tree1a
mapTree f (Leaf1a x)		= Leaf1a (f x)
mapTree f (Node1a x t1 t2)  = (Node1a (f x) (mapTree f t1 ) (mapTree f t2) )


mapadd1a :: Number-> Tree1a -> Tree1a 
mapadd1a k t1 = mapTree (+k) t1 

mapsq1a :: Tree1a -> Tree1a 
mapsq1a t1   = mapTree (^2) t1 

--2d
telopNode :: Tree1b -> Tree1a
telopNode (Leaf1b (x,y))			= Leaf1a (x+y)
telopNode (Node1b (x,y) t1 t2)	 	= Node1a (x+y) (telopNode t1) (telopNode t2)

-- snd 
mapTupTree :: ((Number,Number) -> Number) -> Tree1b -> Tree1a
mapTupTree op (Leaf1b tup)		= Leaf1a (op tup)
mapTupTree op (Node1b tup t1 t2)  = Node1a (op tup) (mapTupTree op t1) $ mapTupTree op t2

tupop = \op tup -> (op (fst tup) (snd tup))

tupadd = mapTupTree (tupop (+) ) --(\tup -> (fst tup) + (snd tup))
tupsub = mapTupTree (tupop (-) ) --(\tup -> (fst tup) - (snd tup))
tupmul = mapTupTree (tupop (*) ) --(\tup -> (fst tup) * (snd tup))


 --3a

binspiegel :: Tree1a -> Tree1a

binspiegel (Leaf1a x)			= Leaf1a x
binspiegel (Node1a x t1 t2)    = Node1a x (binspiegel t2) (binspiegel t1)


-- 3b
{-
data Tree1d = Leaf1d (Number, Number)| Node1d [Tree1d]
	deriving Show
-}
spiegel1d :: Tree1d -> Tree1d

spiegel1d (Leaf1d (x,y))		= Leaf1d (y,x)  
spiegel1d (Node1d  tlist)    	= Node1d (map spiegel1d (reverse tlist))

--4a

{-}
data Tree1c = Leaf1c | Node1c Int Tree1c Tree1c
	deriving Show
-}
a = [1..10]
b = [5,3,7,8,1,2,9,4,6]
c = [5,3,4,7,8,1,2,9,6]

-- moest arguments swappen vanwege foldl. Hoe dat te omzeilen?
t_insert :: Tree1c -> Int -> Tree1c
t_insert (Node1c k t1 t2) x	| x > k 	= Node1c k t1 (t_insert t2 x)
							|otherwise  = Node1c k (t_insert t1 x) t2
t_insert (Leaf1c) x			= (Node1c x Leaf1c Leaf1c)

--4b
makeTree :: [Int] -> Tree1c
makeTree []		= Leaf1c
makeTree xs		= t_insert (makeTree (init xs)) (last xs)

makeTreeFold :: [Int] -> Tree1c
makeTreeFold xs = foldl t_insert Leaf1c xs

--4c

{-
-- deze "serialized" een boom naar een lijst, maar dat was niet de opdracht =p
makeList :: Tree1c -> [Int]
makeList (Node1c k t1 t2)	= k : ((makeList t1) ++ (makeList t2))
makeList (Leaf1c) 			= []
-}

makeList :: Tree1c -> [Int]
makeList (Node1c k t1 t2)  	= (makeList t1) ++ [k] ++ (makeList t2)
makeList Leaf1c				= []

rand = [5,1,8,09,6,34,2,43,5,7,6,4,2,2,456,788,53,2,234,2,6,8]

--4d
listSort :: [Int] -> [Int]
listSort xs		= makeList ( makeTree xs)

--4e
treeSort :: Tree1c -> Tree1c
treeSort tree 	= makeTree ( makeList tree)
--showTreeList [ pp1c t1c , pp1c (treeSort t1c) ]

--5
zoek :: Int -> Tree1c -> Tree1c
zoek n Leaf1c				= Prelude.error ("number "++(show n)++" not in the tree")
zoek n (Node1c k t1 t2) 	| n == k		= (Node1c k t1 t2)
							| n > k			= zoek n t2
							| otherwise     = zoek n t1
-- showTreeList [ pp1c (makeTree rand), pp1c (zoek 8 (makeTree rand))]

--6

--lolwat dubbele recursie?
totDiepte :: Int -> Tree1a -> Tree1a
totDiepte 0 (Node1a n t1 t2) 	= Leaf1a n
totDiepte 0 (Leaf1a n) 			= Leaf1a n
totDiepte d (Leaf1a n) 			= Leaf1a n
totDiepte d (Node1a n t1 t2)    = Node1a n (totDiepte (d-1) t1) (totDiepte (d-1) t2)
--showTreeList [ pp1a t1a, pp1a (totDiepte 1 t1a),pp1a (totDiepte 2 t1a),pp1a (totDiepte 3 t1a),pp1a (totDiepte 5 t1a)]

--7a

-- Hoe match ik op Node1a en Leaf1a tegelijk? misschien kan het want ik wil toch alleen n
-- Lijkt er op dat ik veel te veel defs heb, kan dit korter?
vervang :: Tree1a -> String -> Number -> Tree1a
vervang (Node1a n t1 t2) pad k 	| pad == "" 	= Node1a k t1 t2
vervang (Leaf1a n) pad k 		| pad == "" 	= Leaf1a k
vervang (Leaf1a n) pad k 		| pad /= "" 	= error $ "path too long, at leaf with path "++pad

vervang (Node1a n t1 t2) pad k 	| (head pad) == 'l' = Node1a n (vervang t1 (tail pad) k) t2
vervang (Node1a n t1 t2) pad k 	| (head pad) == 'r' = Node1a n t1 $ vervang t2 (tail pad) k
vervang tree pad k 				= error $ "some error occured, maybe your path \""++pad++"\" is illegal? Only 'l' and 'r' are allowed"
-- showTreeList [ pp1a t1a, pp1a (vervang  t1a "l" 1337), pp1a (vervang  t1a "r" 1337), pp1a (vervang  t1a "rlr" 1337)]

--7b
subboom :: Tree1a -> String -> Tree1a
subboom (Node1a n t1 t2) pad  	| pad == "" 	= Node1a n t1 t2
subboom (Leaf1a n) pad  		| pad == "" 	= Leaf1a n
subboom (Leaf1a n) pad  		| pad /= "" 	= error $ "path too long, at leaf with path "++pad

subboom (Node1a n t1 t2) pad  	| (head pad) == 'l' = subboom t1 (tail pad)
subboom (Node1a n t1 t2) pad  	| (head pad) == 'r' = subboom t2 (tail pad) 
subboom tree pad  				= error $ "some error occured, maybe your path \""++pad++"\" is illegal? Only 'l' and 'r' are allowed"
--  showTreeList [ pp1a t1a, pp1a (subboom  t1a "l" ), pp1a (subboom  t1a "r"), pp1a (subboom t1a "rlr" )]


--7c
--linkerbuur :: Tree1a -> String -> RoseTree
--linkerbuur tree pad		| subboom tree pad 	== (Leaf1a 1) && subboom (init pad) == Leaf1a = pp1a t1a

--8
branchlen :: (Int -> Int -> Int) -> Tree1c -> Int
branchlen op (Node1c n t1 t2) 	= op (1 + (branchlen op t1)) (1 + (branchlen op t2))
branchlen op Leaf1c				= 0

branchmin	= branchlen min
branchmax   = branchlen max



test :: Tree1c -> Bool
test tree 	= ( (branchmax tree) - (branchmin tree) ) <= 1

zipLists :: [a] -> [a] -> [a]
zipLists [] [] 		= []
zipLists [] (x:xs)	= x : zipLists [] xs
zipLists (x:xs) []  = x : zipLists [] xs
zipLists (x:xs) (y:ys)	= x:y:(zipLists xs ys)

divlist xs = (take half xs, drop half xs)
	where half 		= floor  $ (length xs)/2 
--8

balanceer :: Tree1c -> Tree1c
balanceer Leaf1c 			= Leaf1c
balanceer (Node1c n t1 t2)	= (Node1c n (balanceer (makeTree fsthalf))  (balanceer (makeTree sndhalf)) )
		where
			remaining			= (makeList t1) ++ (makeList t2)
			(fsthalf,sndhalf)	= divlist remaining



testy f  = f blar h
			where
				h = 3
				blar = h + 3

