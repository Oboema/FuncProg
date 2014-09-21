import FPPrac
import Debug.Trace
import Data.Char
import qualified Data.List -- only using this for sortBy, gives namespace errors with fpprac for many other useful functions
import RoseTree

ts :: Show a => [a] -> [a]
ts thing	= traceShow thing thing

t :: Show a => String -> a -> a
t str thing	= trace (str ++ (show thing)) thing

data Tree1a = Leaf1a Number | Node1a Number Tree1a Tree1a
	deriving Show

t1a = Node1a 7 
		(Leaf1a 4)
		(Node1a 8
			(Node1a 44
				(Leaf1a 4)
				(Leaf1a 20)
			)
			(Leaf1a 39)
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

pp1c (Leaf1c )			= RoseNode "" []
pp1c (Node1c x t1 t2)	= RoseNode (show x) [pp1c t1, pp1c t2]

--1d
data Tree1d = Leaf1d (Number, Number)| Node1d [Tree1d]
	deriving Show

t1d 	= Node1d [ Node1d [Leaf1d (3,4), Leaf1d (5,9), Node1d [ Leaf1d (66,67), Leaf1d(91,43), Leaf1d (3,3)] ], Leaf1d (5,5), Node1d[ Node1d [ Leaf1d(13,37)]] ]

pp1d (Leaf1d x) 		= RoseNode (show x) []
pp1d (Node1d xs)		= RoseNode "" [ pp1d x | x <- xs]

--2a
