import Data.List.Split (splitOn)

main :: IO()
main = do
  problem <- parseProblem <$> getContents
  print $ solve1 problem
  print $ solve2 problem

parseProblem :: String -> [(Int, Maybe Int)]
parseProblem = snd . foldl parseLine (0,[]) . lines

parseLine :: (Int, [(Int, Maybe Int)]) -> String -> (Int, [(Int, Maybe Int)])
parseLine (lastSeen, acc) input = (this, acc ++ empty ++ [(this, Just value)])
  where parsedInts = map read $ splitOn ": " input
        this = head parsedInts
        value = last parsedInts
        empty = if (lastSeen + 1) >= (this - 1) then []
          else zip [lastSeen+1..this-1] (replicate (lastSeen+1+this-1) Nothing)

severity :: Int -> Int -> Maybe Int -> Int
severity _ _ Nothing = 0
severity offset s (Just range) = if doesScanMe offset s range then range * s else 0

doesScanMe :: Int -> Int -> Int -> Bool
doesScanMe offset s range = (offset+s) `mod` (range * 2 - 2) == 0

scannedWithOffset :: Int -> [(Int, Maybe Int)] -> Bool
scannedWithOffset offset = any (\ (a, b) -> maybe False (doesScanMe offset a) b)

solve1 :: [(Int, Maybe Int)] -> Int
solve1 = sum . map (uncurry $ severity 0)

solve2 :: [(Int, Maybe Int)] -> Int
solve2 input = head $ dropWhile (`scannedWithOffset` input) [0..]
