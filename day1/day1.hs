import Data.Char (digitToInt)

main :: IO ()
main = do
  input <- getLine
  let nums = cycle $ map digitToInt input
  let n = length input
  let offset = div n 2
  -- drop 1 for sol 1
  print . sum . take n $ zipWith f nums (drop offset nums)

f :: (Num t, Eq t) => t -> t -> t
f a b = if a == b then a else 0

