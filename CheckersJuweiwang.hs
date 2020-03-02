{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}

module CheckersJuweiwang (moves,apply_move,red_ai,black_ai
                )
where


import Checkers
import Data.List (sort, delete)

red_ai:: GameState -> Move
red_ai s 
    |Red == (_status s) = legalMoves !! bestIndex
    |GameOver == (_status s) = error "The game has finished!! "
    |otherwise = []
    where
        legalMoves = moves s
        allUptdateGameState = movesS s legalMoves
        allHeuristice = map (minimax (_status s) depth (-9999,9999)) allUptdateGameState
        bestIndex = snd $ maximum (zip allHeuristice [0..])
        depth = 10 

black_ai:: GameState -> Move
black_ai s 
    |Black == (_status s) = legalMoves !! bestIndex
    |GameOver == (_status s) = error "The game has finished!! "
    |otherwise = []
    where
        legalMoves = moves s
        allUptdateGameState = movesS s legalMoves
        allHeuristice = map (minimax (_status s) depth (-9999,9999)) allUptdateGameState
        bestIndex = snd $ maximum (zip allHeuristice [0..])
        depth = 10 

black_heuristic :: GameState -> Int
black_heuristic s = length (_blackPieces s) - length (_redPieces s) + 2 * (length (_blackKings s) - length (_redKings s) ) 

red_heuristic:: GameState -> Int
red_heuristic s = length (_redPieces s) - length (_blackPieces s) + 2 * (length (_redKings s) - length (_blackKings s) ) 

gameOver_heuristic:: GameState -> Int
gameOver_heuristic s = case (_status s) of
    Red -> red_heuristic s
    Black -> black_heuristic s  


movesS :: GameState -> [Move] -> [GameState]
movesS s [] = []
movesS s (x:xs) = (apply_move x s):movesS s xs 

minimax :: Status -> Int -> (Int,Int) -> GameState -> Int
minimax me 0 _ s = case me of 
    Red -> red_heuristic s
    Black -> black_heuristic s
    GameOver -> gameOver_heuristic s
minimax me depth (alpha,beta) s 
    |me == (_status s) = pruningMax me depth (alpha,beta) (movesS s (moves s)) 
    |otherwise = pruningMin me depth (alpha,beta) (movesS s (moves s)) 
minimax me _ _ s 
    |me == GameOver = gameOver_heuristic s 


pruningMax:: Status -> Int -> (Int,Int) -> [GameState] -> Int
pruningMax _ _ (alpha,_) [] = alpha 
pruningMax me depth (alpha,beta) (x:xs)
    |alpha >= beta = alpha
    |otherwise = if alpha' > alpha then pruningMax me depth (alpha',beta) xs 
                 else pruningMax me depth (alpha,beta) xs 
    where 
        alpha' = minimax me (depth - 1) (alpha,beta) x 

pruningMin:: Status -> Int -> (Int,Int) -> [GameState] -> Int
pruningMin _ _ (_,beta) [] = beta
pruningMin me depth (alpha,beta) (x:xs)
    |alpha >= beta = beta
    |otherwise = if beta' < beta then pruningMin me depth (alpha,beta') xs
                 else pruningMin me depth (alpha,beta) xs
    where
        beta' = minimax me (depth - 1) (alpha,beta) x 

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

{-delete2 :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
delete2 x [] -> []
delete2 x (c:cs)
    | c == (((ahead1 ahead x)+(ahead1 second1 x)) `div` 2, ((end2 ahead x)+(end2 second1 x)) `div` 2)
      = delete2 -}


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


simple_moves :: GameState -> [Move]
simple_moves s = case (_status s) of
    Red -> map (\(x,y) -> [x,y]) (sort (legalmovespieces ++ legalmoveskings)) 
          where
            newpieces        = map (\(x,y) -> (x+1, y-1)) redpieces ++ 
                               map (\(x,y) -> (x-1, y-1)) redpieces
            allmovespieces   = zip (redpieces ++ redpieces) newpieces
            legalmovespieces = filter (\(_, (x,y)) -> x >= 0 && x <= 7 &&  not ((x,y) `elem` allpieces)) allmovespieces
            newkings         = map (\(x,y) -> (x+1, y-1)) redkings ++ 
                               map (\(x,y) -> (x-1, y-1)) redkings ++
                               map (\(x,y) -> (x+1, y+1)) redkings ++ 
                               map (\(x,y) -> (x-1, y+1)) redkings        
            allmovekings     = zip (redkings ++ redkings ++ redkings ++ redkings) newkings
            legalmoveskings  = filter (\(_, (x,y)) -> x >= 0 && x <= 7 && y >= 0 && y <= 7 && not ((x,y) `elem` allpieces)) allmovekings
    Black -> map (\(x,y) -> [x,y]) (sort (legalmovespieces ++ legalmoveskings)) 
          where
            newpieces        = map (\(x,y) -> (x+1, y+1)) blackpieces ++ 
                               map (\(x,y) -> (x-1, y+1)) blackpieces
            allmovespieces   = zip (blackpieces ++ blackpieces) newpieces
            legalmovespieces = filter (\(_, (x,y)) -> x >= 0 && x <= 7 &&  not ((x,y) `elem` allpieces)) allmovespieces
            newkings         = map (\(x,y) -> (x+1, y-1)) blackkings ++ 
                               map (\(x,y) -> (x-1, y-1)) blackkings ++
                               map (\(x,y) -> (x+1, y+1)) blackkings ++ 
                               map (\(x,y) -> (x-1, y+1)) blackkings        
            allmovekings     = zip (blackkings ++ blackkings ++ blackkings ++ blackkings) newkings
            legalmoveskings  = filter (\(_, (x,y)) -> x >= 0 && x <= 7 && y >= 0 && y <= 7 &&  not ((x,y) `elem` allpieces)) allmovekings
    _ -> []
  where
    redpieces   = _redPieces s
    blackpieces = _blackPieces s
    redkings    = _redKings s
    blackkings  = _blackKings s
    allpieces   = redpieces ++ blackpieces ++ redkings ++ blackkings


jump_moves :: GameState -> [Move]
jump_moves s = case (_status s) of
    Red   -> legalpiecesmoves ++ legalkingsmoves
      where
        allpiecesmoves   = concatMap (\x -> jump_pieces [(delete_piece s Red x, [x])] Red []) redpieces
        legalpiecesmoves = filter (\x -> length x > 1) allpiecesmoves
        allkingsmoves    = concatMap (\x -> jump_kings [(delete_piece s Red x, [x])] Red []) redkings
        legalkingsmoves  = filter (\x -> length x > 1) allkingsmoves
    Black -> legalpiecesmoves ++ legalkingsmoves
      where
        allpiecesmoves   = concatMap (\x -> jump_pieces [(delete_piece s Black x, [x])] Black []) blackpieces
        legalpiecesmoves = filter (\x -> length x > 1) allpiecesmoves
        allkingsmoves    = concatMap (\x -> jump_kings [(delete_piece s Black x, [x])] Black []) blackkings
        legalkingsmoves  = filter (\x -> length x > 1) allkingsmoves
    _     -> []
  where
    redpieces   = _redPieces s
    blackpieces = _blackPieces s
    redkings    = _redKings s
    blackkings  = _blackKings s
    allpieces   = redpieces ++ blackpieces ++ redkings ++ blackkings

moves :: GameState -> [Move]
moves s 
    | jumps == [] = simples
    | otherwise   = jumps
  where
    simples = simple_moves s 
    jumps = jump_moves s


-- helper function 

jump_one_piece :: (GameState,Move) -> Status -> [(GameState,Move)]
jump_one_piece (s, (x,y):t) status = case status of 
  Red   | y == 0    -> jump_one_king (s, (x,y):t) status -- Crowning!
        | otherwise -> sAndmove1 ++ sAndmove2
          where
            sAndmove1 = if legal_jump s status (x,y) (x+2, y-2) then [(delete_piece s Black (x+1, y-1), (x+2, y-2):(x,y):t)] else []
            sAndmove2 = if legal_jump s status (x,y) (x-2, y-2) then [(delete_piece s Black (x-1, y-1), (x-2, y-2):(x,y):t)] else []
  Black | y == 7    -> jump_one_king (s, (x,y):t) status -- Crowning!
        | otherwise -> sAndmove1 ++ sAndmove2
          where
            sAndmove1 = if legal_jump s status (x,y) (x+2, y+2) then [(delete_piece s Red (x+1, y+1), (x+2, y+2):(x,y):t)] else []
            sAndmove2 = if legal_jump s status (x,y) (x-2, y+2) then [(delete_piece s Red (x-1, y+1), (x-2, y+2):(x,y):t)] else []
  _     -> []


jump_one_king :: (GameState, Move) -> Status -> [(GameState,Move)]
jump_one_king (s, (x,y):t) status = case status of 
  Red     -> sAndmove1 ++ sAndmove2 ++ sAndmove3 ++ sAndmove4
          where
            sAndmove1 = if legal_jump s status (x,y) (x+2, y-2) then [(delete_piece s Black (x+1, y-1), (x+2, y-2):(x,y):t)] else []
            sAndmove2 = if legal_jump s status (x,y) (x-2, y-2) then [(delete_piece s Black (x-1, y-1), (x-2, y-2):(x,y):t)] else []
            sAndmove3 = if legal_jump s status (x,y) (x+2, y+2) then [(delete_piece s Black (x+1, y+1), (x+2, y+2):(x,y):t)] else []
            sAndmove4 = if legal_jump s status (x,y) (x-2, y+2) then [(delete_piece s Black (x-1, y+1), (x-2, y+2):(x,y):t)] else []
  Black  -> sAndmove1 ++ sAndmove2 ++ sAndmove3 ++ sAndmove4
          where
            sAndmove1 = if legal_jump s status (x,y) (x+2, y-2) then [(delete_piece s Red (x+1, y-1), (x+2, y-2):(x,y):t)] else []
            sAndmove2 = if legal_jump s status (x,y) (x-2, y-2) then [(delete_piece s Red (x-1, y-1), (x-2, y-2):(x,y):t)] else []
            sAndmove3 = if legal_jump s status (x,y) (x+2, y+2) then [(delete_piece s Red (x+1, y+1), (x+2, y+2):(x,y):t)] else []
            sAndmove4 = if legal_jump s status (x,y) (x-2, y+2) then [(delete_piece s Red (x-1, y+1), (x-2, y+2):(x,y):t)] else []
  _     -> []

legal_jump :: GameState -> Status -> Coord -> Coord -> Bool
legal_jump s status (x,y) (nx,ny) = x >= 0 && x <= 7 && y >= 0 && y <= 7 && nx >= 0 && nx <= 7  && ny >= 0 && ny <= 7 && 
  case status of 
    Red   -> inter `elem` blacks && not ((nx,ny) `elem` allpieces)
    Black -> inter `elem` reds && not ((nx,ny) `elem` allpieces)
  where
    inter       = ((x + nx) `div` 2, (y + ny) `div` 2)
    redpieces   = _redPieces s
    blackpieces = _blackPieces s
    redkings    = _redKings s
    blackkings  = _blackKings s
    reds        = redpieces ++ redkings
    blacks      = blackpieces ++ blackkings
    allpieces   = redpieces ++ blackpieces ++ redkings ++ blackkings

jump_pieces :: [(GameState, Move)] -> Status -> [Move] -> [Move]
jump_pieces [] _ acc = acc
jump_pieces (x:xs) status acc 
      | nexts == []  = jump_pieces xs status (reverse (snd x) : acc) 
      | otherwise    = jump_pieces (nexts ++ xs) status acc 
   where
    nexts = jump_one_piece x status 

jump_kings :: [(GameState, Move)] -> Status -> [Move] -> [Move]
jump_kings [] _ acc = acc
jump_kings (x:xs) status acc 
      | nexts == []  = jump_kings xs status (reverse (snd x) : acc) 
      | otherwise    = jump_kings (nexts ++ xs) status acc 
   where
    nexts = jump_one_king x status 

delete_piece :: GameState -> Status -> Coord -> GameState
delete_piece (GameState bp rp bk rk st m) status x = case status of 
    Red   -> GameState bp (delete x rp) bk (delete x rk) st m
    Black -> GameState (delete x bp) rp (delete x bk) rk st m
    _     -> error ""
