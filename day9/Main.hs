{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (sum)

main :: IO ()
main = do
  result <- foldl func initial <$> getContents
  print result

data Solution = Solution{depth :: Int, sum :: Int, isGarbage :: Bool, skipChar :: Bool, garbage :: Int} deriving (Show)

initial = Solution{depth=1, sum=0, isGarbage=False, skipChar=False, garbage = 0}

func :: Solution -> Char -> Solution
func s@Solution{..} c
  | skipChar = s{skipChar = False}
  | c == '!' = s{skipChar = True}
  | isGarbage = if c == '>' then s{isGarbage = False} else s{garbage = garbage + 1}
  | c == '<' = s{isGarbage = True}
  | c == '{' = s{depth=depth+1, sum = sum + depth}
  | c == '}' = s{depth=depth-1}
  | otherwise = s
