module LibSpec where

import Test.Hspec

import Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solution" $ do
    it "example should be solved for fist problem" $ do
      solve1 [0,3,0,1,-3] `shouldBe` 5
    it "example should be solved for second problem" $ do
      solve2 [0,3,0,1,-3] `shouldBe` 10
    
