badFermat :: Integer
badFermat = check 0 
       where check x = if isPrime (2^(2^x)+1) then check(x+1) else x
             isPrime n = if n > 1 then null [x|x<-[2 .. (floor$sqrt$fromIntegral n)], n `mod` x == 0] else False



is_prime :: Integer -> Bool
is_prime 1 = False
is_prime n = if n > 1 then null [x|x<-[2 .. (floor$sqrt$fromIntegral n)], n `mod` x == 0] else False

data SF a = SS a
          | FF
   deriving (Show, Eq)

changeMode :: [a] -> SF [a]
changeMode [] = FF
changeMode x = SS x