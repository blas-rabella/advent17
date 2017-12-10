{-# LANGUAGE RecordWildCards #-}

module Main (main
            , Acc(..)
            , step
            , solve1
            , solve2
            , initial) where

import qualified Data.List.Split as SP
import Data.Char
import Data.Bits
import Numeric (showHex)
import qualified Data.Text as T

strip  = T.unpack . T.strip . T.pack

main :: IO ()
main = do
  text <- getContents
  print . solve1 initial $ map (read) . SP.splitOn "," $ text
  print $ solve2 initial $  strip text

data Acc = Acc{xs :: [Int], idx :: Int, sk :: Int} deriving (Show, Eq)

initial :: Acc
initial = Acc{xs=[0..255], idx=0, sk=0}

solve2 :: Acc -> String -> String
solve2 acc s = concatMap (toHex) . compact $ sparseHash acc s
  where toHex n = if n > 15 then showHex n "" else "0" ++ showHex n ""

solve1 :: Acc -> [Int] -> Int
solve1 acc ns = product . take 2 . xs $ foldl step acc ns

sparseHash :: Acc -> String -> [Int]
sparseHash acc s = xs . foldl step acc . concat . replicate 64 $ getBytes s

compact :: [Int] -> [Int]
compact = map (foldl (xor) 0) . SP.chunksOf 16

getBytes :: String -> [Int]
getBytes s = map (ord) s ++ suffix
  where suffix = [17,31,73,47,23]

step :: Acc -> Int -> Acc
step zero@Acc{..} n = zero{xs=newList, idx=newIdx, sk=sk+1}
  where newList = pinchAndTwist idx n xs
        newIdx = mod (idx+n+sk) (length xs)

pinchAndTwist :: Int -> Int -> [Int] -> [Int]
pinchAndTwist idx len xs = pre ++ post
  where (interval, rest) = splitAt len . drop idx $ xs ++ xs
        (post, pre) = splitAt (ns - idx) . take ns $ reverse interval ++ rest
        ns = length xs
