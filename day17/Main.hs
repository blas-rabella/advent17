
import Data.List (foldl')
import qualified Data.Sequence as S

main :: IO ()
main = do
  print $ genIdx 328

f step idx seq val = (nextIdx+1, S.insertAt (nextIdx+1) val seq)
  where nextIdx = (idx+step) `mod` (S.length seq)

g step idx oldVal val = if nextIdx+1 == 1 then (nextIdx+1, val) else (nextIdx+1, oldVal)
  where nextIdx = (idx+step) `mod` val

genList step = foldl (\(idx, l) val -> f step idx l val) (0, S.fromList [0]) [1..2017]
genIdx step = foldl' (\(idx, oldVal) val -> g step idx oldVal val) (0,0) [1..50000000]

