{-# LANGUAGE RecordWildCards #-}

module Main (main
            , Acc(..)
            , step
            , initial) where

import qualified Data.List.Split as SP
import Data.Char
import Data.Bits
import Data.Graph
import qualified Data.Matrix as M

import qualified Data.Text as T

strip  = T.unpack . T.strip . T.pack

main :: IO ()
main = do
  let keys = buildKeys "xlqgujun"
  let matrix = map knotHash keys
  print $ solve1 matrix

solve1 :: [[Int]] -> Int
solve1 = countBits

countBits :: [[Int]] -> Int
countBits = sum . map (sum . map popCount)  

buildKeys :: String -> [String]
buildKeys s = map (\n -> (s ++ "-") ++ show n) [0..127]

type Key = (Int, Int, Int)




--- TODO: MOVE THIS TO MODULE

data Acc = Acc{xs :: [Int], idx :: Int, sk :: Int} deriving (Show, Eq)

initial :: Acc
initial = Acc{xs=[0..255], idx=0, sk=0}

knotHash :: String -> [Int]
knotHash = compact . sparseHash initial

sparseHash :: Acc -> String -> [Int]
sparseHash acc s = xs . foldl step acc . concat . replicate 64 $ getBytes s

compact :: [Int] -> [Int]
compact = map (foldl xor 0) . SP.chunksOf 16

getBytes :: String -> [Int]
getBytes s = map ord s ++ suffix
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
