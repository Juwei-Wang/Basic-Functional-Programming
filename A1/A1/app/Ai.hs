module Ai where

import Data.List (sort, delete)

--import Tui
import Checkers
import Moves
import Main

{-30053278 juweiwang-}
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

