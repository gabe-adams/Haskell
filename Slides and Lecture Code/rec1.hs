-- sample definitions for the Recursion slides
-- Version 2: 26 January 2016

fact :: Integer -> Integer
fact n
     | n==0 = 1
     | n>0  = n * fact (n-1)

fib  :: Integer -> Integer
fib  n
    | n==0      = 0
    | n==1      = 1
    | n>1       = fib(n-1) + fib(n-2)
    | otherwise = error "fib given a negative argument"

sum' :: (Num a) => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

maximum' :: (Ord a) => [a] -> a 
maximum' [] = error "maximum of empty list!"   -- (1)
maximum' [x] = x                               -- (2)
maximum' (x:xs) = max x (maximum' xs)          -- (3)

-- replicate' n x = a list of n copies of x                  
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x:replicate' (n-1) x                 

-- take' n xs = the first n elements of xs
take' :: Int -> [a] -> [a]
take' n xs | n < 0 = error "negative argument"
take' 0 _          = []
take' _ []         = []
take' n (x:xs)     = x:take' (n-1) xs

-- reverse' xs = the reverse of xs
reverse', reverse'' :: [a] -> [a] 

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' xs = helper xs []
    where
      helper [] ans = ans
      helper (x:xs) ans = helper xs (x:ans)
                      

-- zip' xs ys = the zip of xs and ys
zip' :: [a] -> [b] -> [(a,b)] 
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- (elem' x xs) tests if x is an element of xs              
elem':: (Eq a) => a -> [a] -> Bool 
elem' x [] = False
elem' x (y:ys)
    | x == y    = True
    | otherwise = elem' x ys

elem'':: (Eq a) => a -> [a] -> Bool 
elem'' x xs = helper xs 
    where helper [] = False
--          helper (x:ys) = True -- WRONG
          helper (y:ys) = if (x==y) then True else helper ys