{-# LANGUAGE OverloadedLists #-}
module IntCodeInterpreterSpec where

import PreludeAoC
import IntCodeInterpreter
import Test.Hspec
import Test.QuickCheck

import qualified Data.Vector.Unboxed as Vector

spec :: Spec
spec = do
  describe "stops" $ do
    it "stops at code 99" $ do
      eval [] [99] `shouldBe` ([], [99])

  describe "addition" $ do
    it "adds values" $ do
      let codes = [1101, 2, 3, 3, 99]
      eval [] codes `shouldBe` ([], [1101, 2, 3, 5, 99])
    it "adds values at addresses" $ do
      let codes = [1, 0, 4, 3, 99]
      eval [] codes `shouldBe` ([], [1, 0, 4, 100, 99])
    it "1st example day2" $ do
      let codes = [1, 0, 0, 0, 99]
      eval [] codes `shouldBe` ([], [2, 0, 0, 0, 99])

  describe "multiplication" $ do
    it "multiplies values" $ do
      let codes = [1102, 2, 3, 3, 99]
      eval [] codes `shouldBe` ([], [1102, 2, 3, 6, 99])
    it "multiplies values at addresses" $ do
      let codes = [2, 0, 4, 3, 99]
      eval [] codes `shouldBe` ([], [2, 0, 4, 2*99, 99])

  describe "store" $ do
    it "stores value at position" $ do
      let codes = [3, 2, 0]
      eval [InterpreterInput 99] codes `shouldBe` ([], [3, 2, 99])

  describe "output" $ do
    it "outputs value at specified address" $ do
      let codes = [4, 2, 99]
      eval [] (Vector.toList codes) `shouldBe` ([InterpreterOutput 99], codes)
    it "outputs specified value" $ do
      let codes = [104, 2, 99]
      eval [] (Vector.toList codes) `shouldBe` ([InterpreterOutput 2], codes)

  describe "cat" $ do
    it "returns the input" $ property $ \i -> do
      eval' [InterpreterInput i] [3,0,4,0,99]
        `shouldBe` [InterpreterOutput i]

  describe "Using position mode, consider whether the input is equal to 8;\
       \ output 1 (if it is) or 0 (if it is not)." $ do
    it "is equal to 8" $ do
      eval' [8] [3,9,8,9,10,9,4,9,99,-1,8] `shouldBe` [1]

    it "is not equal to 8" $ do
      eval' [9] [3,9,8,9,10,9,4,9,99,-1,8] `shouldBe` [0]

  describe "Using position mode, consider whether the input is less than 8;\
           \ output 1 (if it is) or 0 (if it is not)." $ do
    it "is less than 8" $ do
      eval' [7] [3,9,7,9,10,9,4,9,99,-1,8] `shouldBe` [1]
    it "is 8" $ do
      eval' [8] [3,9,7,9,10,9,4,9,99,-1,8] `shouldBe` [0]
    it "is greater than 8" $ do
      eval' [9] [3,9,7,9,10,9,4,9,99,-1,8] `shouldBe` [0]

  describe "Using immediate mode, consider whether the input is equal to 8;\
           \ output 1 (if it is) or 0 (if it is not)." $ do
    it "is equal to 8" $ do
      eval' [8] [3,3,1108,-1,8,3,4,3,99] `shouldBe` [1]
    it "is not equal to 8" $ do
      eval' [88] [3,3,1108,-1,8,3,4,3,99] `shouldBe` [0]

  describe "Using immediate mode, consider whether the input is less than 8;\
           \ output 1 (if it is) or 0 (if it is not)." $ do
    it "is less than 8" $ do
      eval' [7] [3,3,1107,-1,8,3,4,3,99] `shouldBe` [1]
    it "is 8" $ do
      eval' [8] [3,3,1107,-1,8,3,4,3,99] `shouldBe` [0]
    it "is greater than 8" $ do
      eval' [9] [3,3,1107,-1,8,3,4,3,99] `shouldBe` [0]

  describe "is zero using position mode" $ do
    it "is 0" $ do
      eval' [0] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] `shouldBe` [0]
    it "isn't 0" $ do
      eval' [111] [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] `shouldBe` [1]

  describe "is zero using immediate mode" $ do
    it "is 0" $ do
      eval' [0] [3,3
                ,1105,-1,9,1101,0,0,12,4,12,99,1] `shouldBe` [0]
    it "isn't 0" $ do
      eval' [77] [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] `shouldBe` [1]
