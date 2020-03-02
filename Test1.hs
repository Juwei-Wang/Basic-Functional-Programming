import Data.List


unzip1::[(a,b)] ->([a],[b])
unzip1 [] =([],[])
unzip1 ((a,b) :xs) = case unzip1 xs of (as,bs) -> (a:as,b:bs)

split_list :: (a -> Bool) ->[a] -> ([a],[a])
split_list f [] = ([],[])
split_list f x = (filter f x,filter f x)

merge :: (Ord a) => [a] -> [a] -> [a]
merge l [] = l
merge [] r = r
merge (a:as) (b:bs) 
    | a < b = a:merge as (b:bs)
    | otherwise = b: merge (a:as) bs

odd_even_split :: [a] -> ([a],[a])
odd_even_split ls = (foldl run ([],[]) (zip [1..] ls))
      where run (as,bs) (x,y)
                | odd x = ((y:as), bs)
                | otherwise = (as, (y:bs))

     

sublists :: [a] -> [[a]]
sublists [] = []
sublists (x:xs) = [x] :foldr f [] (sublists xs)
    where f y z = y : (x : y) : z



{- 
sublists [4,5]
sublists [4,5] succeed [4/x [5]/xs] = [4]: foldr f [] [5]
sublists [5] succeed [5/x] []/xs = [5]: foldr f [] []
f [] [] = []:(5:[]):[]
f 
-}

{-
grow2 :: String -> String
grow2 x = foldl run [] (zip [1..] x)
            where run [] ([],[]) = []
                  run a (b,y) = replicate' y b ++ a-}

grow3 :: String ->String
grow3 xs = concatMap (\(x,y) -> replicate x y) (zip [1..] xs)

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


substring :: String -> String -> Bool
substring s1 s2 = run s1 s1 s2
    where run _ _ [] = False
          run _ [] _ = True
          run s (a:as) (b:bs)
                |a == b = run s as bs
                |otherwise = run s s bs

                
check::Eq a => [a]->[a]->Bool
check l s = check1 l s True where
    check1 _ [] h          = True
    check1 [] _ h          = False
    check1 (x:xs) (y:ys) h = (y == x && check1 xs ys False) || (h && check1 xs (y:ys) h)