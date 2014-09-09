module HC3 where


fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fibH a b 0 =  b
fibH a b i =  fibH (a+b) a (i-1)

fib' = fibH 1 0


-- -------------------------------------------

idml n = foldl f [] [0..n-1]
	where
	  f m i = (1 : replicate i 0) : map (0:) m



idmr n = foldr f [] [n-1,n-2..0]
	where
	  f i m = (1 : replicate i 0) : map (0:) m


-- -------------------------------------------


h x = let
	fac 0 = 1
	fac n = n * fac (n-1)
	b = x+2
      in
	fac b


-- -------------------------------------------


testFoldInfL = take 5 (foldl f [] [1..])
	where
	  f as x = as ++ [x]



testFoldInfR = take 5 (foldr f [] [1..])
	where
	  f x as = [x] ++ as





data NorC = N Int | C Char


f (N x) = show x
f (C x) = [x]

-- -------------------------------------------


data Natnum = Zero
	    | Succ Natnum
	deriving Show



telop x Zero     = x
telop x (Succ y) = Succ (telop x y)


nat2num Zero     = 0
nat2num (Succ x) = 1 + nat2num x

num2nat 0 = Zero
num2nat n = Succ (num2nat (n-1))





-- -----------------------------------------

data List t = Nil
	    | Cons t (List t)
	deriving Show


transfL []     = Nil
transfL (x:xs) = Cons x (transfL xs)


total Nil = 0
total (Cons x rest) = x + total rest



-- -----------------------------------------



data Tree = Leaf Int
	  | Node Int Tree Tree
	  deriving Show



exTree = Node 7 (Node 9 (Leaf 3)
			(Leaf 5) )

		(Node 8 (Node 12 (Leaf 15)
				 (Leaf 20) )

			(Leaf 30) )




-- test = showTree $ toRoseTree exTree
