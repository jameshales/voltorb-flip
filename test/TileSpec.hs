module TileSpec (spec) where

import Control.Exception (ErrorCall, evaluate)
import Data.List (nub, sort)
import Test.Hspec
import Test.QuickCheck

import Tile

instance Arbitrary Tile where
  arbitrary = elements tiles

outOfBoundsError :: Selector ErrorCall
outOfBoundsError = errorCall "Tile out of bounds"

spec :: Spec
spec = do
  describe "instance Enum Tile" $ do
    describe "succ" $ do
      it "returns an error on maxBound" $ do
        evaluate (succ (maxBound :: Tile)) `shouldThrow` outOfBoundsError
    describe "pred" $ do
      it "returns an error on minBound" $ do
        evaluate (pred (minBound :: Tile)) `shouldThrow` outOfBoundsError
    describe "toEnum" $ do
      context "when the value is less than 0" $ do
        it "returns an error" $ property $ do
          i <- choose (minBound, 0)
          return $ evaluate (toEnum i :: Tile) `shouldThrow` outOfBoundsError
      context "when the value is greater than 3" $ do
        it "returns an error" $ property $ do
          i <- choose (4, maxBound)
          return $ evaluate (toEnum i :: Tile) `shouldThrow` outOfBoundsError
      context "otherwise" $ do
        it "is inverted by fromEnum" $ property $ do
          i <- choose (0, 3)
          return $ (fromEnum $ (toEnum i :: Tile)) `shouldBe` i
    describe "fromEnum" $ do
      it "is inverted by toEnum" $ property $
        \t -> (toEnum $ fromEnum t :: Tile) `shouldBe` t

  describe "tile" $ do
    context "when the value is less than 0" $ do
      it "returns an error" $ property $ do
        i <- choose (minBound, 0)
        return $ evaluate (tile i) `shouldThrow` outOfBoundsError
    context "when the value is greater than 3" $ do
      it "returns an error" $ property $ do
        i <- choose (4, maxBound)
        return $ evaluate (tile i) `shouldThrow` outOfBoundsError
    context "otherwise" $ do
      it "is inverted by unTile" $ property $ do
        i <- choose (0, 3)
        return $ unTile (tile i) `shouldBe` i

  describe "isOptional" $ do
    context "given a 0-Tile" $ do
      it "returns False" $
        isOptional (tile 0) `shouldBe` False
    context "given a 1-Tile" $ do
      it "returns False" $
        isOptional (tile 1) `shouldBe` True
    context "given a 2-Tile" $ do
      it "returns True" $
        isOptional (tile 2) `shouldBe` False
    context "given a 3-Tile" $ do
      it "returns True" $
        isOptional (tile 3) `shouldBe` False

  describe "isRequired" $ do
    context "given a 0-Tile" $ do
      it "returns False" $
        isRequired (tile 0) `shouldBe` False
    context "given a 1-Tile" $ do
      it "returns False" $
        isRequired (tile 1) `shouldBe` False
    context "given a 2-Tile" $ do
      it "returns True" $
        isRequired (tile 2) `shouldBe` True
    context "given a 3-Tile" $ do
      it "returns True" $
        isRequired (tile 3) `shouldBe` True

  describe "tiles" $ do
    it "returns a list of 4 tiles" $ do
      length tiles `shouldBe` 4
    it "returns a list of distinct tiles" $ do
      nub tiles `shouldBe` tiles
    it "returns a list of tiles in sorted order" $ do
      sort tiles `shouldBe` tiles

  describe "sumOfTiles" $ do
    context "given an empty list of Tiles" $
      it "returns 0" $
        sumOfTiles [] `shouldBe` 0
    context "given a non-empty list of Tiles" $
      it "adds the value of the Tile at the head of the list to the sumOfTiles of the tail of the list" $ property $
        \t ts -> sumOfTiles (t:ts) `shouldBe` (unTile t + sumOfTiles ts)

  describe "numberOfVoltorbs" $ do
    context "given an empty list of Tiles" $
      it "returns 0" $ do
        numberOfVoltorbs [] `shouldBe` 0
    context "given a non-empty list of Tiles" $ do
      context "if the head of the list is a Voltorb" $
        it "returns the numberOfVoltorbs in the tail of the list" $ property $
          \t ts -> t /= voltorb ==> numberOfVoltorbs (t:ts) `shouldBe` numberOfVoltorbs ts
      context "if the head of the list isn't a Voltorb" $
        it "adds 1 to the numberOfVoltorbs in the tail of the list" $ property $
          \ts -> numberOfVoltorbs (voltorb:ts) `shouldBe` numberOfVoltorbs ts + 1
