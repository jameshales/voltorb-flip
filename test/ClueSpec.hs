module ClueSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Clue

instance Arbitrary Clue where
  arbitrary = do
    s <- choose (0, 15)
    n <- choose (0, 5)
    return $ clue s n

spec :: Spec
spec = do
  describe "clue" $ do
    describe "given a valid sum of tiles and number of Voltorbs" $ do
      it "is inverted by unClue" $ property $ do
        s <- choose (0, 15)
        n <- choose (0, 5)
        return $ unClue (clue s n) `shouldBe` (s, n)
    describe "given a negative sum of tiles" $ do
      it "returns an error" $ property $ do
        s <- choose (minBound, -1)
        n <- choose (0, 5)
        return $ evaluate (clue s n) `shouldThrow` errorCall "Sum of Tiles is less than 0"
    describe "given a negative number of Voltorbs" $ do
      it "returns an error" $ property $ do
        s <- choose (0, 15)
        n <- choose (minBound, -1)
        return $ evaluate (clue s n) `shouldThrow` errorCall "Number of Voltorbs is less than 0"
    describe "given a sum of Tiles greater than 15" $ do
      it "returns an error" $ property $ do
        s <- choose (16, maxBound)
        n <- choose (0, 5)
        return $ evaluate (clue s n) `shouldThrow` errorCall "Sum of Tiles is greater than 15"
    describe "given a number of Voltorbs greater than 5" $ do
      it "returns an error" $ property $ do
        s <- choose (0, 15)
        n <- choose (6, maxBound)
        return $ evaluate (clue s n) `shouldThrow` errorCall "Number of Voltorbs is greater than 5"

  describe "getSumOfTiles" $ do
    it "returns the sum of Tiles from a Clue" $ property $ do
      \c -> getSumOfTiles c `shouldBe` fst (unClue c)

  describe "getNumberOfVoltorbs" $ do
    it "returns the number of Voltorbs from a Clue" $ property $ do
      \c -> getNumberOfVoltorbs c `shouldBe` snd (unClue c)
