{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Void
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec
import Data.Maybe (fromMaybe, fromJust)
import Data.Map.Strict (Map)
import Data.List
import qualified Data.Map.Strict as M
import Data.Function (on)

data Program = Program{name::String, weight::Int, sons::[String]} deriving (Show)
type Parser = Parsec Void String

parser :: Parser Program
parser = do
  name <- many letterChar
  weightz <- string " (" *> decimal <* string ")"
  let weight = fromInteger weightz
  sonz <- optional $ string " -> " *> many letterChar `sepBy` string ", "
  let sons = fromMaybe [] sonz
  return Program{..}

toMap :: [Program] -> Map String Program
toMap = foldr (\p h -> M.insert (name p) p h) M.empty

findRoot :: Map String Program -> String
findRoot m = head $ M.keys m \\ concatMap sons (M.elems m)

nodeValue :: String -> Map String Program -> (Int, Bool)
nodeValue key m
  | null $ sons node = (weight node, True)
  | otherwise = if balancedSons son_values
    then (weight node + sumSubtrees, True)
    else balance son_values key m
      where son_values = map (`nodeValue` m) (sons node)
            sumSubtrees = sum $ map fst son_values
            node = m M.! key
            balancedSons l = length l * fst (head l) == sumSubtrees && all snd l

balance :: [(Int, Bool)] -> String -> Map String Program  -> (Int, Bool)
balance sons_values key m = if null false_son
  then (weight (m M.! unbalanced_key) + (expected - unbalanced_value),False)
  else head false_son
  where
    pairs = zip (sons (m M.! key)) sons_values
    sg :: [[(String,(Int,Bool))]]
    sg = sortBy (compare `on` length) $ groupBy ((==) `on` snd) pairs
    expected = fst . snd. head . last $ sg
    unbalanced_key = fst . head . head $ sg
    unbalanced_value = fst . snd . head . head $ sg
    false_son = filter (not . snd) sons_values

main :: IO ()
main = do
  text <- getContents
  let m = toMap . map (fromJust . parseMaybe parser) . lines $ text
  let root = findRoot m
  print root
  print $ balance (map (`nodeValue` m) (sons (m M.! root))) root m
