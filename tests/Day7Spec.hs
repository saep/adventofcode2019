module Day7Spec where

import PreludeAoC
import Test.Hspec

import Day7
import IntCodeInterpreter

spec :: Spec
spec = do
  pure ()
  describe "solutions" $ do
    it "solution 1" $ do
      solution1 `shouldReturn` Just 18812

    it "solution 2" $ do
      solution2 `shouldReturn` Just 25534964

  describe "example programs" $ do
    it "example 1" $ do
      let code = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
      snd (runCode code [4,3,2,1,0]) `shouldBe` 43210

    it "example 2" $ do
      let code = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23
                 ,4,23,99,0,0]
      snd (runCode code [0,1,2,3,4]) `shouldBe` 54321

    it "example 3" $ do
      let code = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7
                 ,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
      snd (runCode code [1,0,4,3,2]) `shouldBe` 65210
