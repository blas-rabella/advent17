module Main where

import Lib

main :: IO ()
main = do
  text <- getContents
  let nums = map read . lines $ text :: [Int]
  --print $ solve1 nums
  print $ solve2 nums
