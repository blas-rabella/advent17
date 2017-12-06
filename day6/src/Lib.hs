module Lib
    ( solve1
    , solve2
    , redistribute
    ) where

import qualified Data.Vector as V
import Data.Vector.Mutable (write)
import qualified Data.Set as S
import qualified Data.Sequence as Seq

solve1 :: [Int] -> Int
solve1 l = go 0 S.empty $ V.fromList l
  where
    go :: Int -> S.Set (V.Vector Int) -> V.Vector Int -> Int
    go count seen blocks = case (S.member blocks seen) of
      True -> count
      False -> go (count+1) (S.insert blocks seen) (next)
        where next = redistribute blocks

--solve2 :: [Int] -> (Int, Int)
solve2 l = go Seq.empty $ V.fromList l
  where
    --go :: Seq.Seq (V.Vector Int) -> V.Vector Int -> Int
    go seen blocks = case (Seq.elemIndexL blocks seen) of
      Just n -> (Seq.length seen) - n
      Nothing -> go (seen Seq.|> blocks) next
        where next = redistribute blocks
              

redistribute :: V.Vector Int -> V.Vector Int
redistribute blocks = V.accum (+) (V.modify (\v -> write v index 0) blocks) (generate (blocks V.! index) (V.length blocks) index)
  where index = V.maxIndex blocks
 
generate :: Int -> Int -> Int -> [(Int,Int)]
generate value n index = zip indexes numbers
  where indexes = [index+1..n-1] ++ [0..index]
        numbers = replicate remainder (quotient + 1) ++ (replicate (n-remainder) quotient)
        (quotient, remainder) = quotRem value n
