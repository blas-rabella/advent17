{-# LANGUAGE ConstrainedClassMethods, RecordWildCards #-}

module Main where

import Control.Monad.State.Lazy

main :: IO ()
main = do
  line <- getLine
  let n = read line :: Int
  print $ execState (sequence $ replicate (n-1) mNext) MyState{pos=(0,0), mem=[((0,0),1)], direction=R}
  -- print $ evalState (solve2 n) MyState{pos=(0,0), mem=[((0,0),1)], direction=R}

data Direction = R | U | L | D deriving(Show, Eq, Enum, Bounded)

class Circular a where
  next :: Enum a => a -> a
  previous :: Enum a => a -> a
  
instance Circular Direction where
  next a = if a == maxBound then minBound else succ a
  previous a = if a == minBound then maxBound else pred a

type Pos = (Int, Int)
type Cell = (Pos, Int)

-- data MyState = MyState{pos :: Pos, step :: Int, direction :: Direction} deriving(Show)

data MyState = MyState{pos :: Pos, mem :: [Cell], direction :: Direction} deriving(Show)

isCorner (x, y)
 | x + y == 2*x = True
 | x == abs(y) = True
 | x + y == 1 && x <= 0 = True
 | otherwise = False

getNextPos :: Pos -> Direction -> Pos
getNextPos (x, y) U = (x+1, y)
getNextPos (x, y) D = (x-1, y)
getNextPos (x, y) R = (x, y+1)
getNextPos (x, y) L = (x, y-1)

genNext MyState{..} = MyState{pos=nextPos, mem=nextStep, direction=nextDirection}
  where nextDirection = if isCorner nextPos then next direction else direction
        --nextStep = (nextPos, nextValue) : mem
        nextStep = []
        nextPos = getNextPos pos direction
        nextValue = computeValue nextPos mem

mNext :: State MyState ()      
mNext = do modify genNext

solve2 :: Int -> State MyState Int
solve2 n = do
  modify genNext
  st <- get
  let val = snd. head . mem $ st
  if val > n then return val
    else mNext2 n
  

isSolution MyState{..} = (snd . head $ mem) > 289326 

isAdjacent :: Pos -> Pos -> Bool
isAdjacent pos1 pos2 = dist pos1 pos2 < 2.0

dist (a, b) (c, d) = sqrt(fromIntegral((a-c)^2+(b-d)^2))
  
computeValue :: Pos -> [Cell] -> Int
computeValue _ [] = 1
computeValue pos mem = sum . map snd $ filter (isAdjacent pos . fst) mem

