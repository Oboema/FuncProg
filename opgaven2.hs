import FPPrac
import Debug.Trace


--opgave 1

myfilter :: ( a -> Bool ) -> [a] -> [a]
myfilter f xs	= [ k | k <- xs, f k]


myfoldl :: ( a -> a -> a ) -> a -> [a] -> a
myfoldl f z []		= z
myfoldl f z (x:xs)	= myfoldl f (f z x) xs

--myfoldr :: (a -> b -> a ) -> a -> [b] -> a
myfoldr :: (a -> a -> a ) -> a -> [a] -> a
myfoldr f z []		= z
myfoldr f z (x:xs)	= f x (myfoldr xs)