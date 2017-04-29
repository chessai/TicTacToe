module TicTacToe
  (
    size
  , Player
  , Cell
  , Board
  , emptyBoard
  , move
  , bestMove
  ) where

import Prelude hiding (all, elem, head, flip, map, maximum, minimum, tail)
import Data.Maybe
import Data.Matrix
import Data.Vector hiding (reverse, toList)

import VectorUtils

size :: Int
size = 3

data Player = Xer | Oer | None | NoneAndDraw
            deriving (Eq, Read, Show)

data Cell = X | O | Empty
          deriving (Eq, Read)

instance Show Cell where
  show cell =
    case cell of Empty -> "_"
                 X     -> "X"
                 O     -> "O"

playerToCell :: Player -> Cell
playerToCell p =
  case p of None -> Empty
            Xer  -> X
            Oer  -> O

cellToPlayer :: Cell -> Player
cellToPlayer c =
  case c of Empty -> None
            X     -> Xer
            O     -> Oer

type Board = Matrix Cell

-- an Empty Tic-Tac-Toe Board
emptyBoard :: Board
emptyBoard = matrix size size $ \(i,j) -> Empty

move :: (Int, Int) -> Cell -> Board -> Board
move (r, c) cell board = setElem cell (r, c) board

successors :: Player -> Board -> Vector (Board)
successors player board = Data.Vector.fromList [move (r,c) (playerToCell player) board | r <- [1..size], c <-[1..size], getElem r c board == Empty]

boardVector :: Matrix a -> Vector (Vector a)
boardVector b = Data.Vector.concat [rows b, cols b, diags b]
  where rows m  = map (row m) (enumFromN 1 size)
        cols m  = map (col m) (enumFromN 1 size)
        diags m = Data.Vector.fromList [mainDiag m, antiDiag m]
        row m i = getRow i m
        col m i = getCol i m
        mainDiag m = getDiag m
        antiDiag m = getDiag $ flipMatrix m

winner :: Board -> Player
winner b
       | line /= Nothing                    = (cellToPlayer $ head (fromJust line))
       | Empty `elem` (getMatrixAsVector b) = None
       | otherwise                          = NoneAndDraw
       where line = find (allEqual Empty) (boardVector b)

eval :: Player -> Board -> Int
eval player board
     | w == Xer         = 1
     | w == Oer         = -1
     | w == NoneAndDraw = 0
     | player == Xer    = maximum (map (eval Oer) (successors Xer board))
     | otherwise        = minimum (map (eval Xer) (successors Oer board))
     where w = winner board

bestMove :: Board -> Board
bestMove board = argmax (eval Oer) (successors Xer board)
