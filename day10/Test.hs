module Test where

import Test.Hspec
import Main (Acc(..), step, solve1, solve2, initial)

main :: IO ()
main = hspec spec

ex1 :: Acc
ex1 = Acc{xs=[0..4], idx=0, sk=0}
ex2 = Acc{xs=[2,1,0,3,4], idx=3, sk=1}
ex3 = Acc{xs=[4,3,0,1,2], idx=3, sk=2}
ex4 = Acc{xs=[4,3,0,1,2], idx=1, sk=3}
ex5 = Acc{xs=[3,4,2,1,0], idx=4, sk=4}

spec :: Spec
spec = do
  describe "day 10 solution" $ do
    it "steps must work properly" $ do
      scanl step ex1 [3,4,1,5] `shouldBe` [ex1,ex2,ex3,ex4,ex5]
    it "Should solve the problem" $ do
      solve1 ex1 [3,4,1,5] `shouldBe` 12
    it "Should hash properly" $ do
      solve2 initial "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
      solve2 initial "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
      solve2 initial "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
      solve2 initial "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"
