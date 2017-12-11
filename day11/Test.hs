module Test where

import Test.Hspec
import Main (solve1)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "day 10 solution" $ do
    it "should work for the examples" $ do
      solve1 "ne,ne,ne" `shouldBe` 3
      solve1 "ne,ne,sw,sw" `shouldBe` 0
      solve1 "ne,ne,s,s" `shouldBe` 2
      solve1 "se,sw,se,sw,sw" `shouldBe` 3
      
