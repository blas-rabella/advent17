import Control.Applicative

main :: IO ()
main = do
  text <- getContents
  let numbers = map (map read . words) $ lines text :: [[Int]]
  print . sum . map f2 $ numbers

solve1 :: [Int] -> Int
solve1 ns = a - b
  where (a,b) = foldl f (minBound :: Int ,maxBound :: Int) ns

f :: (Int, Int) -> Int -> (Int, Int)
f (a, b) x
  | (a < x) && (b > x) = (x,x)
  | a < x = (x, b)
  | b > x = (a, x)
  | otherwise = (a, b) 

f2 l = fst . head . filter (\t -> fst t /=1 && snd t == 0) $ quotRem <$> l <*> l
