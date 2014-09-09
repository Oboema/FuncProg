import Data.Char
import Data.List



myCurry f = \x y -> f (x,y)

myUncurry f = \(x,y) -> f x y


f x y = x+ 2*y


-- perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [  x:p  | x <- xs , p <- perms (xs\\[x]) ]



-- matrix stuff
m  = [ [1,2,3], [4,5,6] ]

m' = [ [1,2], [3,4], [5,6] ]




-- matrix addition
mAdd :: Num a => [[a]] -> [[a]] -> [[a]]
mAdd = zipWith (zipWith (+)) 



zipWith2 = zipWith.zipWith




-- variants of transpose

transp, transp', transp'' :: [[a]] -> [[a]]



transp ([]:_) = []
transp  m     = map head m : transp (map tail m)



transp' []     = repeat []
transp' (r:rs) = zipWith (:) r rs'
		where
		  rs' = transp' rs



transp'' = foldr (zipWith (:)) (repeat [])











-- vector multiplication
xs <.> ys = foldl (+) 0 zs
	where
	  zs = zipWith (*) xs ys





-- matrix-vector multiplication
m <**> ys = map (<.>ys) m





	-- matrix-matrix multiplication
m0 <***> m1 = transpose (map (m0<**>) (transpose m1))











