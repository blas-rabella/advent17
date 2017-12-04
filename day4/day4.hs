import Data.List (nub, sort)

main :: IO ()
main = do
  text <- getContents
  print . sum . map (isValid . words) . lines $ text -- sol 1
  print . sum . map (isValid . map sort . words) . lines $ text -- sol 2

isValid :: [String] -> Int
isValid l = if num_elems == num_unique then 1 else 0
  where num_elems = length l
        num_unique = length $ nub l
        
