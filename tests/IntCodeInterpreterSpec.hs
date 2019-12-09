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
      eval [99] [] `shouldBe` (PureCPU [99] 0 0, [])

  describe "addition" $ do
    it "adds values" $ do
      let codes = [1101, 2, 3, 3, 99]
      eval codes [] `shouldBe` (PureCPU [1101, 2, 3, 5, 99] 4 0, [])
    it "adds values at addresses" $ do
      let codes = [1, 0, 4, 3, 99]
      eval codes [] `shouldBe` (PureCPU [1, 0, 4, 100, 99] 4 0, [])
    it "1st example day2" $ do
      let codes = [1, 0, 0, 0, 99]
      eval codes [] `shouldBe` (PureCPU [2, 0, 0, 0, 99] 4 0, [])

  describe "multiplication" $ do
    it "multiplies values" $ do
      let codes = [1102, 2, 3, 3, 99]
      eval codes [] `shouldBe` (PureCPU [1102, 2, 3, 6, 99] 4 0, [])
    it "multiplies values at addresses" $ do
      let codes = [2, 0, 4, 3, 99]
      eval codes [] `shouldBe` (PureCPU [2, 0, 4, 2*99, 99] 4 0, [])

  describe "store" $ do
    it "stores value at position" $ do
      let codes = [3, 2, 0]
      eval codes [InterpreterInput 99] `shouldBe` (PureCPU [3, 2, 99] 2 0, [])

  describe "output" $ do
    it "outputs value at specified address" $ do
      let codes = [4, 2, 99]
      eval (Vector.toList codes) [] `shouldBe` (PureCPU codes 2 0, [InterpreterOutput 99])
    it "outputs specified value" $ do
      let codes = [104, 2, 99]
      eval (Vector.toList codes) [] `shouldBe` (PureCPU codes 2 0, [InterpreterOutput 2])

  describe "input" $ do
    it "stores value relative to relative base" $ do
      let codes = [109,1,203,0,99] 
      eval codes [1337] `shouldBe` (PureCPU [109,1337,203,0,99] 4 1, [])

  describe "cat" $ do
    it "returns the input" $ property $ \i -> do
      eval' [3,0,4,0,99] [InterpreterInput i]
        `shouldBe` [InterpreterOutput i]

  describe "Using position mode, consider whether the input is equal to 8;\
       \ output 1 (if it is) or 0 (if it is not)." $ do
    it "is equal to 8" $ do
      eval' [3,9,8,9,10,9,4,9,99,-1,8] [8] `shouldBe` [1]

    it "is not equal to 8" $ do
      eval' [3,9,8,9,10,9,4,9,99,-1,8] [9] `shouldBe` [0]

  describe "Using position mode, consider whether the input is less than 8;\
           \ output 1 (if it is) or 0 (if it is not)." $ do
    it "is less than 8" $ do
      eval' [3,9,7,9,10,9,4,9,99,-1,8] [7] `shouldBe` [1]
    it "is 8" $ do
      eval' [3,9,7,9,10,9,4,9,99,-1,8] [8] `shouldBe` [0]
    it "is greater than 8" $ do
      eval' [3,9,7,9,10,9,4,9,99,-1,8] [9] `shouldBe` [0]

  describe "Using immediate mode, consider whether the input is equal to 8;\
           \ output 1 (if it is) or 0 (if it is not)." $ do
    it "is equal to 8" $ do
      eval' [3,3,1108,-1,8,3,4,3,99] [8] `shouldBe` [1]
    it "is not equal to 8" $ do
      eval' [3,3,1108,-1,8,3,4,3,99] [88] `shouldBe` [0]

  describe "Using immediate mode, consider whether the input is less than 8;\
           \ output 1 (if it is) or 0 (if it is not)." $ do
    it "is less than 8" $ do
      eval' [3,3,1107,-1,8,3,4,3,99] [7] `shouldBe` [1]
    it "is 8" $ do
      eval' [3,3,1107,-1,8,3,4,3,99] [8] `shouldBe` [0]
    it "is greater than 8" $ do
      eval' [3,3,1107,-1,8,3,4,3,99] [9] `shouldBe` [0]

  describe "is zero using position mode" $ do
    it "is 0" $ do
      eval' [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0] `shouldBe` [0]
    it "isn't 0" $ do
      eval' [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [111] `shouldBe` [1]

  describe "is zero using immediate mode" $ do
    it "is 0" $ do
      eval' [3,3 ,1105,-1,9,1101,0,0,12,4,12,99,1] [0] `shouldBe` [0]
    it "isn't 0" $ do
      eval' [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [77] `shouldBe` [1]


  describe "large number" $ do
    it "should output 16 digit number" $ do
      let code = [1102,34915192,34915192,7,4,7,99,0]
      map (\(InterpreterOutput i) -> length (show i)) (eval' code []) `shouldBe` [16]

  describe "adjust relative base" $ do
    it "example 1" $ do
      eval [109,22,99] [] `shouldBe` (PureCPU [109,22,99] 2 22, [])
    it "example 2" $ do
      eval [109,1,109,-1,99] [] `shouldBe` (PureCPU [109,1,109,-1,99] 4 0, [])
    it "example 3" $ do
      eval [9,1,209,1,99] [] `shouldBe` (PureCPU [9,1,209,1,99] 4 210, [])

  describe "memory adjustment" $ do
    it "copy" $ do
      let code = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
      map (\(InterpreterOutput o) -> o) (eval' code []) `shouldBe` code
