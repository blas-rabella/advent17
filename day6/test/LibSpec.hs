module LibSpec where

import Test.Hspec
import Lib
import Data.Vector (fromList)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day6" $ do
    it "Blocks should be redistributed properly" $ do
      redistribute (fromList [0,2,7,0]) `shouldBe` fromList [2,4,1,2]
      redistribute (fromList [2,4,1,2]) `shouldBe` fromList [3,1,2,3]
      redistribute (fromList [3,1,2,3]) `shouldBe` fromList [0,2,3,4]
      redistribute (fromList [0,2,3,4]) `shouldBe` fromList [1,3,4,1]
      redistribute (fromList [1,3,4,1]) `shouldBe` fromList [2,4,1,2]
    it "example should be solved for first problem" $ do
      solve1 [0,2,7,0] `shouldBe` 5
    it "example should be solved for second problem" $ do
      solve2 [0,2,7,0] `shouldBe` 4
