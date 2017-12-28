{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Matrix as M
import Data.List.Split (splitOn)
import Control.Arrow
import Control.Monad.State.Strict
import Prelude hiding (Left, Right)
import Data.Char (isAlpha)
import Control.Monad.Loops (iterateWhile)

type Pos = (Int, Int)
data Direction = Up | Down | Left | Right deriving (Show, Eq)
type Grid = M.Matrix Char
data Problem = Problem{pos :: Pos, grid :: Grid, dir :: Direction, nodes :: String, count::Int} deriving (Show, Eq)

type MyState = State Problem

main :: IO ()
main = do
  text <- lines <$> getContents
  let solution = solve1 text
  print . reverse . nodes .snd $ solution
  print . count . snd $ solution

solve1 text = runState (iterateWhile not move) Problem{..}
  where grid = M.fromLists text
        pos = findStart (head text)
        dir = Down
        nodes = ""
        count = -1

findStart :: String -> Pos
findStart s = (1, col+1)
  where col = length $ takeWhile (/= '|') s

nextPos :: Direction -> Pos -> Pos
nextPos Up = first (\x -> x - 1)
nextPos Down = first (\x -> x + 1)
nextPos Right = second (+ 1)
nextPos Left = second (\x -> x - 1)

turn :: MyState ()
turn = do
  s <- get
  let direction = dir s
  let isVertical = direction == Up || direction == Down
  let newDirection = getTurn (pos s) isVertical (grid s)
  put s{dir=newDirection}
  where
    getTurn :: Pos -> Bool -> Grid -> Direction
    getTurn (0,_) False _ = Down -- First row can only go Down
    getTurn (_,0) True _ = Right -- First column can only go Right
    getTurn (i,j) False m = if isPath (m M.! (i-1,j)) then Up else Down
    getTurn (i,j) True m = if isPath (m M.! (i,j-1)) then Left else Right

move :: MyState Bool
move = do
  s <- get
  -- now we are in the position, we might need to push to nodes, turn or keep going.
  let character = grid s M.! pos s
  go character 
--  return (fst newPos > M.nrows (grid s) || snd newPos > M.ncols (grid s))
  s2 <- get
  let newPos = nextPos (dir s2) (pos s2)
  let newCount = (count s2) + 1
  put s2{pos=newPos, count=newCount}
  return (character == ' ')
    where go :: Char -> MyState ()
          go '+' = turn
          go x
           | isAlpha x = do
               s <- get
               put s{nodes=x:nodes s}
           | otherwise = return ()
                
      
isPath :: Char -> Bool
isPath '-' = True
isPath '|' = True
isPath x = isAlpha x 
        
