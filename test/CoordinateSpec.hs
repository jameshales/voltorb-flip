module CoordinateSpec (spec) where

import Control.Exception (ErrorCall, evaluate)
import Data.List (nub, sort)
import Test.Hspec
import Test.QuickCheck

import Coordinate

instance Arbitrary Coordinate where
  arbitrary = elements coordinates

outOfBoundsError :: Selector ErrorCall
outOfBoundsError = errorCall "Coordinate out of bounds"

spec :: Spec
spec = do
  describe "instance Enum Coordinate" $ do
    describe "succ" $ do
      it "returns an error on maxBound" $ do
        evaluate (succ (maxBound :: Coordinate)) `shouldThrow` outOfBoundsError
    describe "pred" $ do
      it "returns an error on minBound" $ do
        evaluate (pred (minBound :: Coordinate)) `shouldThrow` outOfBoundsError
    describe "toEnum" $ do
      context "when the value is less than 0" $ do
        it "returns an error" $ property $ do
          i <- choose (minBound, 0)
          return $ evaluate (toEnum i :: Coordinate) `shouldThrow` outOfBoundsError
      context "when the value is greater than 4" $ do
        it "returns an error" $ property $ do
          i <- choose (5, maxBound)
          return $ evaluate (toEnum i :: Coordinate) `shouldThrow` outOfBoundsError
      context "otherwise" $ do
        it "is inverted by fromEnum" $ property $ do
          i <- choose (0, 4)
          return $ (fromEnum $ (toEnum i :: Coordinate)) `shouldBe` i
    describe "fromEnum" $ do
      it "is inverted by toEnum" $ property $
        \c -> (toEnum $ fromEnum c :: Coordinate) `shouldBe` c

  describe "coordinate" $ do
    context "when the value is less than 0" $ do
      it "returns an error" $ property $ do
        i <- choose (minBound, 0)
        return $ evaluate (coordinate i) `shouldThrow` anyException
    context "when the value is greater than 4" $ do
      it "returns an error" $ property $ do
        i <- choose (5, maxBound)
        return $ evaluate (coordinate i) `shouldThrow` anyException
    context "otherwise" $ do
      it "is inverted by unCoordinate" $ property $ do
        i <- choose (0, 4)
        return $ unCoordinate (coordinate i) `shouldBe` i

  describe "coordinates" $ do
    it "returns a list of 5 coordinates" $ do
      length coordinates `shouldBe` 5
    it "returns a list of distinct coordinates" $ do
      nub coordinates `shouldBe` coordinates
    it "returns a list of coordinates in sorted order" $ do
      sort coordinates `shouldBe` coordinates
