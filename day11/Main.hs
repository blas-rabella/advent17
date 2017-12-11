module Main (solve1, main) where

import Data.List.Split (splitOn)

main = do
  text <- getLine
  print $ solve1 text
  print $ solve2 text

type Cell = (Int, Int, Int)

initial :: Cell
initial = (0, 0, 0)

nextCell :: Cell -> String -> Cell
nextCell (x, y, z) "n" = (x,y+1,z-1)
nextCell (x, y, z) "s" = (x,y-1,z+1)
nextCell (x, y, z) "ne" = (x+1,y,z-1)
nextCell (x, y, z) "nw" = (x-1,y+1,z)
nextCell (x, y, z) "se" = (x+1,y-1,z)
nextCell (x, y, z) "sw" = (x-1,y,z+1)

distance :: Cell -> Cell -> Int
distance (a1,b1,c1) (a2,b2,c2) = (abs (a1 - a2) + abs (b1 - b2) +abs (c1 - c2)) `div` 2

solve1 :: String -> Int
solve1 line = distance initial endCell
  where endCell = foldl nextCell initial $ splitOn "," line

solve2 :: String -> Int
solve2 line = maximum $ map (distance initial) path
  where path = scanl nextCell initial $ splitOn "," line
