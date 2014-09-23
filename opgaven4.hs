import FPPrac
import Debug.Trace
import Data.Char
import qualified Data.List -- only using this for sortBy, gives namespace errors with fpprac for many other useful functions
import qualified FPPrac.Trees.RoseTree as RT
import qualified FPPrac.Trees.ParseTree as PT

extree  = RT.showTree RT.exampleTree

exampleParseTree = PT.showTree PT.exampleTree

--1a
data Unit        = Nil
    deriving Show
data Bintree a b = Leaf b | Node a (Bintree a b) (Bintree a b)
    deriving Show

--1b

type Tree1a     = Bintree Number Number
type Tree1b     = Bintree (Number, Number) (Number, Number)
type Tree1c     = Bintree Int Unit
--1c
pp :: (Show a, Show b) => Bintree a b -> PT.ParseTree
pp (Leaf b)         = PT.ParseNode (show b) []
pp (Node a t1 t2)   = PT.ParseNode (show a) [pp t1, pp t2]

rand = [5,1,8,09,6,34,2,43,5,7,6,4,2,2,456,788,53,2,234,2,6,8]

-- testing
t_insert :: Tree1c -> Int -> Tree1c
t_insert (Node k t1 t2) x   | x > k     = Node k t1 (t_insert t2 x)
                            |otherwise  = Node k (t_insert t1 x) t2
t_insert (Leaf Nil)  x        = (Node x (Leaf Nil) (Leaf Nil))


makeList :: Tree1c -> [Int]
makeList (Node k t1 t2)   = (makeList t1) ++ [k] ++ (makeList t2)
makeList (Leaf Nil)       = []

makeTree :: [Int] -> Tree1c
makeTree []     = (Leaf Nil)
makeTree xs     = t_insert (makeTree (init xs)) (last xs)

treeSort :: Tree1c -> Tree1c
treeSort tree   = makeTree ( makeList tree)
--showTreeList [ pp (makeTree rand) , pp (treeSort (makeTree rand)) ]

-- 2
k = "((3+((4*4)/3)))-5)"

data Parens = LPar | RPar  -- Now how Do I say LPar must equal "(" and RPar must equal ")" ?
    deriving Show

data Op     = Add | Sub | Mul | Div | Pow   -- Same for the operators.
    deriving Show

data Nummer  = G --How do I avoid writing One | Two | etc?
    deriving Show

data Expr       = Nummer 
                | Parens Expr Op Expr Parens

data MyParseTree    = Bintree Op Nummer


parseSimple :: String -> MyParseTree
parseSimple ('(':xs)        = parseSimple xs
parseSimple ((Nummer n):xs) = Node(parseSimple xs) (Leaf n) ( parseSimple xs
