module TileSpec (spec) where

import Control.Exception (evaluate)
import Data.List (nub, sort)
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances ()
import Tile

spec :: Spec
spec = do
  describe "instance Enum Tile" $ do
    describe "succ" $ do
      it "returns an error on maxBound" $ do
        evaluate (succ (maxBound :: Tile)) `shouldThrow` anyException
    describe "pred" $ do
      it "returns an error on minBound" $ do
        evaluate (pred (minBound :: Tile)) `shouldThrow` anyException
    describe "toEnum" $ do
      context "when the value is less than 0" $ do
        it "returns an error" $ property $ do
          i <- choose (minBound, 0)
          return $ evaluate (toEnum i :: Tile) `shouldThrow` anyException
      context "when the value is greater than 3" $ do
        it "returns an error" $ property $ do
          i <- choose (4, maxBound)
          return $ evaluate (toEnum i :: Tile) `shouldThrow` anyException
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
        return $ evaluate (tile i) `shouldThrow` anyException
    context "when the value is greater than 3" $ do
      it "returns an error" $ property $ do
        i <- choose (4, maxBound)
        return $ evaluate (tile i) `shouldThrow` anyException
    context "otherwise" $ do
      it "is inverted by unTile" $ property $ do
        i <- choose (0, 3)
        return $ unTile (tile i) `shouldBe` i

  describe "tiles" $ do
    it "returns a list of 4 tiles" $ do
      length tiles `shouldBe` 4
    it "returns a list of distinct tiles" $ do
      nub tiles `shouldBe` tiles
    it "returns a list of tiles in sorted order" $ do
      sort tiles `shouldBe` tiles

  describe "sumOfTiles" $ do
    it "returns 0 for an empty list" $ do
      sumOfTiles [] `shouldBe` 0
    it "increments the sum by the value of a tile as the tile is consed to the given list" $ property $
      \t ts -> sumOfTiles (t:ts) `shouldBe` (unTile t + sumOfTiles ts)

  describe "numberOfVoltorbs" $ do
    it "returns 0 for an empty list" $ do
      numberOfVoltorbs [] `shouldBe` 0
    it "returns the same count if a non-Voltorb tile is consed to the given list" $ property $
      \t ts -> t /= voltorb ==> numberOfVoltorbs (t:ts) `shouldBe` numberOfVoltorbs ts
    it "increments the count of a Voltorb tile is consed to the given list" $ property $
      \ts -> numberOfVoltorbs (voltorb:ts) `shouldBe` numberOfVoltorbs ts + 1
