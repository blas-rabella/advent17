import Data.Char (digitToInt)

main :: IO ()
main = do
  input <- getLine
  let nums = cycle $ map digitToInt input
  print . sum . take (length input)$ zipWith f nums (getReady2 (length input) nums)


f :: (Num t, Eq t) => t -> t -> t
f a b = if a == b then a else 0

getReady2 :: Int -> [Int] -> [Int]
getReady2 n = drop (div n 2)

getReady :: [Int] -> [Int]
getReady = tail
