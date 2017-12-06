module Lib (solve1, solve2) where

import Data.Vector

solve1 :: [Int] -> Int
solve1 l = go 0 0 (+ 1) $ fromList l

solve2 :: [Int] -> Int
solve2 l = go 0 0 f2 $ fromList l

f2 :: Int -> Int
f2 x
  | x >= 3 = x - 1
  | otherwise = x + 1

go :: Int -> Int -> (Int -> Int) -> Vector Int -> Int
go index count f v = case (v !? index) of
                     Nothing -> count
                     Just value -> go (index + value) (count + 1) f $ (update v $ singleton (index,f value))
