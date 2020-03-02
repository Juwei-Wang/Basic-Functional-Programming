module Main where

import Data.List (sort, delete)

--import Tui
import Checkers
import Moves

main :: IO ()
main = human apply_move initialGameState


apply_move:: Move -> GameState -> GameState
apply_move move st
    | member (ahead move) (_redKings st) && (_status st)== Red && member1 move (moves st)= 
    st{_redKings= replace (ahead move) (end move) (_redKings st),
    _status= changePlayer (_status st),_message= "legal moves"++show (moves st) ,_blackPieces= delete2 move (_blackPieces st)
    , _blackKings= delete2 move (_blackKings st) }

    | member (ahead move) (_blackKings st) && (_status st)== Black && member1 move (moves st)= 
    st{_blackKings = replace (ahead move) (end move) (_blackKings st),
    _status= changePlayer (_status st),_message= "legal moves"++show (moves st),_redPieces= delete2 move (_redPieces st)
    , _redKings= delete2 move (_redKings st)}

    | member (ahead move) (_redPieces st) && (_status st)== Red && member1 move (moves st)= 
    st{_redPieces= replace (ahead move) (end move) (_redPieces st),
    _status= changePlayer (_status st),_message= "legal moves"++show (moves st),_blackPieces= delete2 move (_blackPieces st)
    , _blackKings= delete2 move (_blackKings st)}

    | member (ahead move) (_blackPieces st) && (_status st)== Black && member1 move (moves st)= 
    st{_blackPieces= replace (ahead move) (end move) (_blackPieces st),
    _status= changePlayer (_status st),_message= "legal moves"++show (moves st),_redPieces= delete2 move (_redPieces st)
    , _redKings= delete2 move (_redKings st)}

    | otherwise = st {_message = "Wrong player or the piece is not moving correctly, please try again"}

member :: (Int,Int) -> [(Int,Int)] -> Bool
member x [] = False
member x (a:as)
    | x==a = True
    |otherwise = member x as 

member1 :: (Eq a) => a -> [a] -> Bool
member1 x [] = False
member1 x (a:as)
    |x==a = True
    |otherwise = member1 x as 

replace :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
replace start end [] = []
replace start end (a:as) 
     | a == start = end:as
     | otherwise = a:(replace start end as)

changePlayer :: Status -> Status
changePlayer s = case s of
    Red -> Black
    Black -> Red

delete1 :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
delete1 (a,b) (d,e) [] = []
delete1 (a,b) (d,e) (c:as)
    | c == ((a+d)`div` 2,(b+e)`div` 2) = delete1 (a,b) (d,e) as
    |otherwise = c:(delete1 (a,b) (d,e) as)

delete2 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
delete2 (x:xs) [] = []
delete2  [] x = x
delete2 (x:xs) c
    | member1 (((ahead1 x) + (ahead1 (ahead xs))) `div` 2, ((end2 x)+(end2 (ahead xs))) `div` 2) c
    = delete (((ahead1 x) + (ahead1 (ahead xs))) `div` 2, ((end2 x)+(end2 (ahead xs))) `div` 2) c 
    |otherwise = delete2 xs c
    
{-delete2 xs ys = filter (\x y -> not(x `elem` ys)) xs (end3 xs)-}
{-delete2 xs as = filter (\x y -> not (((ahead1 x) + (ahead1 y) `div` 2,(end2 x)+(end2 y) `div` 2) `elem` as)) xs (end3 xs)-}



end3 ::  [(Int,Int)] -> [(Int,Int)]
end3 [] =[]
end3 (x:as) = as

ahead1 :: (Int,Int) -> Int
ahead1 (a,b) = a 

end2 :: (Int,Int) -> Int
end2 (a,b) = b

ahead :: [(Int,Int)] -> (Int,Int)
ahead [] = (0,0)
ahead (a:as) = a

end :: [a] -> a
end [x] = x 
end (_:xs) = end xs
end [] = error "Can't do last of an empty list!"

second1 :: [(Int,Int)] -> (Int,Int)
second1 x = head$tail x