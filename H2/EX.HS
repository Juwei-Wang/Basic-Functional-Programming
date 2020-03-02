import Data.List

-- 30053278
-- Juwei WANG

{-1-}

twoTautology:: ((Bool,Bool) -> Bool) -> Bool
twoTautology f 
           |f(True,False) == True && f(True,True) == True && f(False,False) == True && f(False,True) == True = True
           |otherwise = False

twoEquiv::((Bool,Bool)->Bool)->((Bool,Bool)->Bool)->Bool
twoEquiv k g 
       | twoTautology k == twoTautology g = True
       | otherwise = False


{-2-}

nreverse:: (Ord a) => [a]  -> [a] 
nreverse [] = []  
nreverse (x:xs) = (nreverse xs) ++ [x] 

hreverse:: (Ord a) => [a]  -> [a] 
hreverse = foldl (\acc x -> x : acc) [] 

{-3-}

badFermat :: Integer
badFermat = check 1
      where check x
              | is_prime (numberEval x) = check (x+1)
              | otherwise = x


numberEval :: Integer -> Integer
numberEval n = (2 ^ (2 ^ n) + 1)

is_prime :: Integer -> Bool
is_prime 1 = False
is_prime n = if n > 1 then null [x|x<-[2 .. (floor$sqrt$fromIntegral n)], n `mod` x == 0] else False   


{-4-}

qsort :: (a -> a -> Bool) -> [a] -> [a]  
qsort f [] = []  
qsort f (x:xs) =   
    let smallerSorted = qsort f [a | a <- xs, f a x]  
        biggerSorted = qsort f [a | a <- xs, f x a]  
    in  smallerSorted ++ [x] ++ biggerSorted 

bsort :: (a -> a -> Bool) -> [a] -> [a]
bsort f [] = []
bsort f lst
    | hasNoSwaps = lst
    | otherwise  = next ++ [last bubbled]
    where
        next = bsort f (init bubbled)
        (bubbled, hasNoSwaps) = bubble f lst

bubble :: (a -> a -> Bool) -> [a] -> ([a], Bool)
bubble f [x] = ([x], True)
bubble f (x:y:xs)
    | f x y     = (y : bubbled_x, False)
    | otherwise = (x : bubbled_y, swaps_y)
    where
        (bubbled_y, swaps_y) = bubble f (y:xs)
        (bubbled_x, swaps_x) = bubble f (x:xs)

mSort :: (a -> a -> Bool) -> [a] -> [a]
mSort f [] = []
mSort f [a] = [a]
mSort f a =
  merge f (mSort f firstFew) (mSort f lastFew)
    where firstFew = take ((length a) `div` 2) a
          lastFew = drop ((length a) `div` 2) a

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge f a [] = a
merge f [] b = b
merge f (a:as) (b:bs)
  | f a b     = a:(merge f as (b:bs))
  | otherwise = b:(merge f (a:as) bs)



{-5-}
data SF a = SS a
          | FF
   deriving (Show, Eq)


collatz :: Int -> Int
collatz n
      | even n = div n 2
      | otherwise = 3 * n + 1

collatzIndex :: Int -> SF [Int]
collatzIndex n
            |n==1 = SS [1]
            |n<=0 = FF
            |n>=1 = changeMode (collatzIndex1 n)

changeMode :: [a] -> SF [a]
changeMode [] = FF
changeMode x = SS x
 
collatzIndex1 ::Int -> [Int]
collatzIndex1 n
            |n==1 = [1]
            |n<=0 = error "The number should greater than 0"
            |n>=1 = n:collatzIndex1(collatz n)


{-6-}
fact1891 :: Integer
fact1891 = foldl (*) 1 [2,3..1891]



{-7-}
data STree a = Node (STree a)  a (STree a)
                         |  Leaf

type Matrix a = [[a]]
type DoubleMatrix = Matrix Double
                         
