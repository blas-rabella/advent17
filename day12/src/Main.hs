module Main where
import           Data.Graph.Inductive
import           Data.List.Split      (splitOn)

buildEdge :: Int -> Int -> LEdge ()
buildEdge a b = (a,b,())

solve1 :: Graph gr => Node -> gr a () -> Int
solve1 n gr = length $ reachable 0 gr

solve2 :: Graph gr => gr a () -> Int
solve2 gr = length $ scc gr

buildGraph :: [String] -> Gr Int ()
buildGraph text = insEdges edges gr
  where
    gr = insNodes nodes empty
    n = length text
    nodes = zip [0..n-1][0..n-1]
    edges = concatMap readEdges text

readEdges :: String -> [LEdge ()]
readEdges line = map (buildEdge orig) dests
  where orig':dests' = splitOn " <-> " line
        orig = read orig' :: Int
        dests = map read $ splitOn ", " $ concat dests' :: [Int]

main :: IO ()
main = do
  text <- getContents
  let graph = buildGraph $ lines text
  print $ solve1 0 graph
  print $ solve2 graph
