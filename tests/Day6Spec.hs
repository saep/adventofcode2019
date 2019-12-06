{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Day6Spec where

import PreludeAoC
import Test.Hspec

import Day6

spec :: Spec
spec = do
  describe "solutions" $ do
    it "solution 1" $
      solution1 `shouldReturn` 273985
    it "solution 2" $
      solution2 `shouldReturn` 460

  let Just exampleRelations = parseMaybe parseOrbitRelations
        "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
  describe "parser" $ do
    it "parses the example" $
      exampleRelations `shouldBe`
        [ "B" `IsInOrbitAround` CenterOfMass
        , "C" `IsInOrbitAround` "B"
        , "D" `IsInOrbitAround` "C"
        , "E" `IsInOrbitAround` "D"
        , "F" `IsInOrbitAround` "E"
        , "G" `IsInOrbitAround` "B"
        , "H" `IsInOrbitAround` "G"
        , "I" `IsInOrbitAround` "D"
        , "J" `IsInOrbitAround` "E"
        , "K" `IsInOrbitAround` "J"
        , "L" `IsInOrbitAround` "K"
        ]

  describe "calculateOrbits" $ do
    it "is 0 for empty map" $ do
      calculateOrbits empty `shouldBe` mempty
    it "is (0, 1) for single relation map" $ do
      calculateOrbits ["B" `IsInOrbitAround` CenterOfMass]
        `shouldBe` Orbits{ indirect = 0, direct = 1}
    it "is 42 for example map" $ do
      calculateOrbits exampleRelations
        `shouldBe` Orbits{ indirect = 31, direct = 11}

  describe "example for 2nd part" $ do
    it "is 4" $ do
      let rs = parseMaybe parseOrbitRelations
            "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
      fmap lengthOfShortestPath (shortestPath =<< rs) `shouldBe` Just 4
