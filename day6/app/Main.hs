module Main where

import Lib

main :: IO ()
main = do
  text <- getContents
  let nums = map read $ words text
  print . solve1 $ nums
  print . solve2 $ nums
