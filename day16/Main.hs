module Main where

import qualified Data.Vector as V
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)
import Data.List (elemIndices)

spin i v = b V.++ a
  where (a,b) = V.splitAt ((V.length v) - i) v

exchange iA iB v = v V.// updateL
  where updateL = [(iA, b), (iB, a)]
        a = v V.! iA
        b = v V.! iB

partner a b v = v V.// updateL
  where updateL = [(iA, b), (iB, a)]
        iA = fromJust $ V.findIndex (== a) v
        iB = fromJust $ V.findIndex (== b) v
        
parse :: String -> V.Vector Char -> V.Vector Char
parse (op:cs) v
  | op == 'x' = exchange (read iA) (read $ head rest) v
  | op == 's' = spin (read iA) v
  | op == 'p' = partner (head iA) (head . head $ rest) v
  | otherwise = error "FUCK"
  where iA:rest = splitOn "/" cs

solve1 :: [String] -> V.Vector Char
solve1 = foldl (flip $ parse) (V.fromList ['a'..'p'])

solve2 insts = head $ drop (10000000 `mod` period) states
  where
    states = scanl (flip parse) initial insts
    initial = V.fromList ['a'..'p']
    period = elemIndices initial states !! 1

main = do
  text <- splitOn "," <$> getLine
  print $ solve1 text
  print . solve2 . concat $ repeat text
