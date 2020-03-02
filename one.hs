


doubleMe x = x + x

tripleMe x = x + x + x




cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2] 

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0] 

{-count :: (Ord a) => [a] -> Int
count [] = 0
count [x] = 1
count [x:s]
    | x > c  = 1 + count s
    | otherwise = 0 + count s
    where c = maximum1 s -}







take' :: (Num i, Ord i) => i -> [a] -> [a]   
take' n _   
    | n <= 0   = []   
take' _ []     = []   
take' n (x:xs) = x : take' (n-1) xs








lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"  



addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors a b = (fst a + fst b, snd a + snd b) 

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  

first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z 


head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x 

describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list." 

{-maxHead :: [a] -> a
maxHead [] = error "No empty set"
maxHead list = head'$sort list-}

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y 



reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x] 

repeat' :: a -> [a]  
repeat' x = x:repeat' x 

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs  




{-Assignment-}

{-1-}
{-avgThree :: Int -> Int -> Int -> Float
avgThree x y z = fromIntegral ((x + y + z) / 3)-}

{-2-}
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
            |(fact y) > x = (y - 1)
            |otherwise = run x (y+1)

fact :: (Integral a) => a -> a  
fact 0 = 1  
fact n = n * fact (n - 1) 


{-4-}
myGCD :: Integer -> Integer -> Integer
myGCD 0 b 
    | (b > 0) = b
    | otherwise = error "the number must greater than zero"
myGCD a b 
    | (a > 0) && (b > 0) = myGCD (b `mod` a) a
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
grow n = [y | c <- snd$head2$x, b <- fst$head2$x, y = replicate' c b]
    where x = zip' n [1,2..]

zip1 :: [a] -> [b] -> [(a,b)]  
zip1 _ [] = []  
zip1 [] _ = []  
zip1 (x:xs) (y:ys) = (x,y):zip1 xs ys 


replicate' :: Integer -> String -> String   
replicate' n x   
    | n <= 0    = []   
    | otherwise = x ++ replicate' (n-1) x

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

{-myUnzip :: 
myUnzip [] = ([], [])
myUnzip ((a, b):xs) = (a : ???, b : ???)
    where ??? = myUnzip xs-}

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





{-binom2 :: Int -> Int
binom2 0 = 1 
binom2 k = k * (binom2 k-1)-}
             
{-gcd' :: Int -> Int -> Int
gcd' x 0 = (x - (y * c));
gcd' x y 
    | x < 0 || y < 0 = error "The number should be positive"
    | otherwise =  gcd' y (x - (y * c))
    where  
    c = x / y-}

{-binom:: Int -> Int -> Int
binom x y 
    | x < 1 = error "n must equal or greater than 1"
    | y < 0 || y > x = error "k must between 0 and n"
    | otherwise = x / y -}


{-removeNonUppercase :: [char] -> [char]
removeNonUppercase st = [ c | c <- st,  c 'elem' ['A'..'Z']]-}

{-filter1 :: (a -> bool) -> [a] -> [a]
filter1 _ [] = []
filter1 p (x:xs)
    | p x = x : filter1 p xs
    | otherwise = filter1 p xs-}