module Day3Spec where

import PreludeAoC
import Day3
import Test.Hspec

spec :: Spec
spec = do
  describe "solutions" $ do
    it "part 1" $
      solution1 `shouldReturn` Just 2180
    it "part 2" $
      solution2 `shouldReturn` Just 112316

  describe "examples min Manhattan distance" $ do
    it "1st example" $ do
      minManhattanDistanceToOrigin
        [R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72]
        [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]
        `shouldBe` Just 159

    it "2nd example" $ do
      minManhattanDistanceToOrigin
        [R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51]
        [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]
        `shouldBe` Just 135

  describe "examples best steps" $ do
    it "1st example" $ do
      minSumOfWireLengthWithIntersection
        [R 75,D 30,R 83,U 83,L 12,D 49,R 71,U 7,L 72]
        [U 62,R 66,U 55,R 34,D 71,R 55,D 58,R 83]
        `shouldBe` Just 610
    it "2nd example" $ do
      minSumOfWireLengthWithIntersection
        [R 98,U 47,R 26,D 63,R 33,U 87,L 62,D 20,R 33,U 53,R 51]
        [U 98,R 91,D 20,R 16,D 67,R 40,U 7,R 15,U 6,R 7]
        `shouldBe` Just 410
