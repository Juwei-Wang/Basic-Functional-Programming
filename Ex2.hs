import Data.list

data Point = Point Float Float deriving (Show)
data Point = Circle Point Float | Rectangle Point Point deriving (Show) 

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x1+a) (x2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r 

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

myflatten::[[a]] -> [a]
myflatten [] = []
myflatten (xs:xss) = myappend xs (myflatten xss)

mymap:: (a -> b) -> ([a] -> [b])
mymap f [] = []
mymap f (a:as) = (f a):(mymap f as)

inlist:: (a -> Bool) -> [a] -> Bool
inlist pred as = myOR (mymap pred as)

data SF a = SS a | FF
  deriving (Eq,Show)

sfmap:: (a -> b) -> ((SF a) -> (SF b))
sfmap f (SS x) = SS (f x)
sfmap f FF = FF

mymap:: (a -> b) -> ([a] -> [b])
mymap f [] = []
mymap f (a:as) = (f a):(mymap f as)

myfilter:: (a -> Bool) -> [a] -> [a]
myfilter p [] = []
myfilter p (a:as) = if (p a) then a:(myfilter p as)
                  else myfilter p as


myfold:: (a -> c -> c) -> c -> ([a] -> c)
myfold f c [] = c
myfold f c (x:xs) = f x (myfold f c  xs)

sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs 

sum1' :: (Num a) => [a] -> a  
sum1' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

map' :: (a -> b) -> [a] -> [b]  
map' f xs = foldr (\x acc -> f x : acc) [] xs

{-
m
-}
{-1-}
twoTautology:: ((Bool,Bool) -> Bool) -> Bool
twoTautology (Bool,Bool) Bool 
twoTautology f 
twoTautology f Bool


twoEquiv::((Bool,Bool)->Bool)->((Bool,Bool)->Bool)->Bool


taut' :: (Bool, Bool) -> Bool
taut' (_, _) = True

xor' :: (Bool, Bool) -> Bool
xor' (True, True) = True
xor' (True, False) = False
xor' (False, True) = False
xor' (False, False) = True

{-2-}

nreverse:: (Ord a) => [a]  -> [a] 
nreverse [] = []  
nreverse (x:xs) = (nreverse xs) ++ [x] 

freverse:: (Ord a) => [a]  -> [a]


hreverse:: (Ord a) => [a]  -> [a] 
hreverse = foldl (\acc x -> x : acc) [] 

rev :: [a] -> [a]
rev xs = go xs []
   where
   go :: [a] -> [a] -> [a]
   go []     ys = ys
   go (x:xs) ys = go xs (x:ys)

{-3-}

bsort::(a -> a -> Bool) -> [a] -> [a]

qsort::(a -> a -> Bool) -> [a] -> [a]


quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

msort::(a -> a -> Bool) -> [a] -> [a]

{-4-}

badFermat :: Integer
badFermat = mestest n  where  
    mestest n = powertwo (n+1)
    powertwo n    | n == 1 = True
                  | n `mod` 2 == 0 = powertwo (n `div` 2)
                  | otherwise = False

{-5-}
collatz :: Int -> Int
collatzIndex :: Int -> SF [Int]

{-6-}
type Matrix a = [[a]]
type DoubleMatrix = Matrix Double

transpose:: Matrix a -> (Maybe (Matrix a))
addMat :: DoubleMatrix -> DoubleMatrix -> (Maybe DoubleMatrix)
multMat :: DoubleMatrix -> DoubleMatrix -> (Maybe DoubleMatrix)

{-7-}
data STree a = Node (STree a)  a (STree a)
|  Leaf

isAVL: Ord(a) => STree a -> Bool