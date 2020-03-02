--UCID: 30053278
--Name: WANGJUWEI

{-1-}
avgThree :: Int -> Int -> Int -> Float
avgThree x y z =  fromIntegral (x + y + z) / 3

{-2-}
binom1 :: Int -> Int -> Int
binom1 n k = (product' n k) 

binom2 :: Int -> Int 
binom2 n = fact n

maxThree :: Int -> Int -> Int -> (Int,Int)
maxThree x y z = (r,p)
    where r = maximum1 [x,y,z]
          p = count [x,y,z] r

count :: [Int] -> Int -> Int
count [] r = 0
count (x:s) r
    | x == r = 1 + max
    | otherwise = 0 + max
    where max = count s r

maximum1 :: [Int] -> Int
maximum1 [] = error "maximum of empty list"
maximum1 [x] = x 
maximum1 (x:xs)
    | x > maxTail2 = x
    | otherwise = maxTail2 
    where maxTail2 = maximum1 xs

{-3-}
invFac :: Integer -> Integer
invFac n = run n 1 
    where run x y
            | (n < 0) = error "Sorry, the number should not be negative!" 
            |(fact y) > x = (y - 1)
            |otherwise = run x (y+1)

fact :: (Integral a) => a -> a  
fact 0 = 1  
fact n = n * fact (n - 1) 


{-4-}
myGcd :: Integer -> Integer -> Integer
myGcd 0 b 
    | (b > 0) = b
    | otherwise = error "the number must greater than zero"
myGcd a b 
    | (a > 0) && (b > 0) = myGcd (b `mod` a) a
    | otherwise = error "the number must greater than zero"


{-5-}

product' :: Int -> Int -> Int
product' n k = check n k 1  
    where check x y z
            | (n < k) = error "The first number must greater than second number"
            | y == z = x
            | otherwise = x * (check (x-1) y (z+1) )



{-6-}

grow :: String -> String
grow [] = []
grow n = grow1 a
   where a = zip n [1,2..]

grow1 :: [(Char,Integer)] -> String
grow1 [] = []
grow1 ((a,b):xs) = (replicate' b a) ++ (grow1 xs)

zip1 :: [a] -> [b] -> [(a,b)]  
zip1 _ [] = []  
zip1 [] _ = []  
zip1 (x:xs) (y:ys) = (x,y):zip1 xs ys 


replicate' :: Integer -> Char -> String   
replicate' n x   
    | n <= 0    = []   
    | otherwise = x : replicate' (n-1) x

length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs 

head2 :: [a] -> a
head2 (x:xs) = x

tail2 :: String -> String
tail2 (x:xs) = xs


{-7-}
instrictorder::  [Int] -> Bool
instrictorder [] = True
instrictorder (x:xs) = (x > head1 xs) && instrictorder xs

head1 :: [Int] -> Int
head1 (x:xs) = x

{-8-}
cheapItem :: [(String, Int)] -> Int -> [String]
cheapItem list num = [ x | (x,y) <- list, y < num]
 
{-9-}

sortByCost :: [(String, Int)] -> [(String, Int)]
sortByCost list = [(x,y) | (x,y)<- list, c<- quicksort$snd$unzip list, c==y]  {-sort (snd (unzip list))-}
{- [(a,4),(b,3)] ->  ([a,b], [4,3]) ->  [4,3] -> [3,4] -> [(b,3),(a,4)] -}


quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  


{-10-}
divisors :: Integer -> [Integer]
divisors x = check x 1
    where check 0 y = []
          check x y 
            |x < 0 = [] 
            |y > x = []
            |otherwise = if ((mod x y) == 0)
                         then y: check x (y+1)
                         else check x (y+1)


{-11-}
substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
    |  prerequest xs ys = True
    |  substring xs (tail' ys) = True
    |  otherwise = False

prerequest :: String -> String-> Bool
prerequest [] ys = True
prerequest (x:xs) [] = False
prerequest (x:xs) (y:ys) = ((x == y) && (prerequest xs ys))

tail' :: String -> String
tail' [] = []
tail' (x:xs) = xs


{-12-}
sublists :: [a] -> [[a]]
sublists [] = []
sublists (x:xs) = [x] :foldr f [] (sublists xs)
    where f y z = y : (x : y) : z
