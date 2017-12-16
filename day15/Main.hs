module Main where

import Data.List (unfoldr)
import Data.Bits

main :: IO ()
main = do
  a <- read <$> getLine
  b <- read <$> getLine
  -- print $ solve1 a b
  print $ solve2 a b

solve1 :: Int -> Int -> Int
solve1 a b = length . filter id . take 40000000 $ zipWith isSameLow (genList factorA a) (genList factorB b)
solve2 :: Int -> Int -> Int
solve2 a b = length . filter id . take 5000000 $ zipWith isSameLow (filter (\n -> n `mod` 4 == 0) $ genList factorA a) (filter (\n -> n `mod` 8 == 0) $ genList factorB b)
magicNum :: Int
magicNum = 2147483647

factorA :: Int
factorA = 16807

factorB :: Int
factorB = 48271

nextNum :: Integral t => t -> t -> t -> Maybe (t, t)
nextNum factor divisor n = Just (a, a)
  where a = rem (n * factor) divisor

genList :: Int -> Int -> [Int]
genList factor = unfoldr (nextNum factor magicNum)

isSameLow :: (Num a, Bits a) => a -> a -> Bool
isSameLow a b = (a .&. 0xFFFF) == (b .&. 0xFFFF)
