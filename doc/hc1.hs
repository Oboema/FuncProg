import Data.Char


-- f :: Int -> Int		-- f :: Number -> Number
f x = x^2 + 1



ff a b c x = a*x^2 + b*x + c


g (x,y) = f x + y



h x y = f x + y



fun x y | a>0 && b>=0	= a+b
	| otherwise	= a*b
	where
	  a = x+y
	  b = x-y















fac 0 = 1
fac n = n * fac (n-1)


fac'' n = if n==0 then 1 else n * fac'' (n-1)



fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)













hfac 0 r  = r
hfac n r  = hfac (n-1) (n*r )

fac' n = hfac n 1










lengte []	= 0
lengte (x:xs)	= 1 + lengte xs


totaal [] = 0
totaal (x:xs) = x + totaal xs




listapply [] [] = []
listapply (f:fs) (x:xs) = f x : listapply fs xs



