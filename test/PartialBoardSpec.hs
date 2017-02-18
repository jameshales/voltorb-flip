module PartialBoardSpec (spec) where

import Control.Exception (evaluate)
import Data.Array (listArray, (!), (//))
import Data.Function (on)
import Data.List (nubBy)
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances
import Board (tileAt, tilesAt)
import PartialBoard

spec :: Spec
spec = do
  describe "partialBoard" $ do
    context "given a valid Array of Maybe Tiles" $ do
      it "is inverted by unPartialBoard" $ property $ do
        a <- genMaybeTileArray
        return $ unPartialBoard (partialBoard a) `shouldBe` a
    context "given an Array with bounds less than (minBound, maxBound)" $ do
      it "returns an error" $ property $ do
        let invalidBounds (a, b) = a /= minBound || b /= maxBound
        bounds  <- arbitrary `suchThat` invalidBounds
        ts      <- vectorOf 25 arbitrary
        let arr  = listArray bounds ts
        return $ evaluate (partialBoard arr) `shouldThrow` errorCall "Array does not have full bounds"

  describe "emptyBoard" $
    it "contains Nothing" $ property $ do
      \p -> (unPartialBoard emptyBoard) ! p `shouldBe` Nothing

  describe "maybeTileAt" $
    it "returns the Maybe Tile at the given Position" $ property $ do
      \pb p mt -> let pb' = partialBoard (unPartialBoard pb // [(p, mt)]) in maybeTileAt pb' p `shouldBe` mt

  describe "maybeTilesAt" $
    it "returns the Maybe Tiles at the given Positions" $ property $ do
      as <- fmap (nubBy ((==) `on` fst)) arbitrary
      let ps  = map fst as
      let ts  = map snd as
      pb <- fmap (partialBoard . (// as) . unPartialBoard) arbitrary
      return $ maybeTilesAt pb ps `shouldBe` ts

  describe "flipTileAt" $ do
    it "returns the Tile at the given Position in a Board" $ property $ do
      \pb b p -> fst (flipTileAt pb b p) `shouldBe` tileAt b p
    it "flips the Tile at the given Position in a Board" $ property  $ do
      \pb b p -> maybeTileAt (snd $ flipTileAt pb b p) p `shouldBe` Just (tileAt b p)
    it "otherwise leaves the PartialBoard unchanged" $ property $ do
      \pb b p p' -> p /= p' ==> maybeTileAt (snd $ flipTileAt pb b p) p' `shouldBe` maybeTileAt pb p'
    it "preserves the consistency of a PartialBoard" $ property $ do
      (b, pb) <- genConsistentPartialBoard
      p       <- arbitrary
      return $ flipTileAt pb b p `shouldSatisfy` (flip isConsistent b . snd)

  describe "flipTilesAt" $ do
    it "returns the Tiles at the given Positions in a Board" $ property $ do
      \pb b ps -> fst (flipTilesAt pb b ps) `shouldBe` tilesAt b ps
    it "flips the Tiles at the given Positions in a Board" $ property $ do
      \pb b ps -> maybeTilesAt (snd $ flipTilesAt pb b ps) ps `shouldBe` map Just (tilesAt b ps)
    it "otherwise leaves the PartialBoard unchanged" $ property $ do
      \pb b ps p -> not (p `elem` ps) ==> maybeTileAt (snd $ flipTilesAt pb b ps) p `shouldBe` maybeTileAt pb p 
    it "preserves the consistency of a PartialBoard" $ property $ do
      (b, pb) <- genConsistentPartialBoard
      ps      <- arbitrary
      return $ flipTilesAt pb b ps `shouldSatisfy` (flip isConsistent b . snd)

  describe "isConsistent" $ do
    context "given an empty PartialBoard" $
      it "returns True" $ property $ do
        \b -> isConsistent emptyBoard b `shouldBe` True
    context "given a PartialBoard with all of the flipped Tiles equal to the corresponding Tiles in the given Board" $
      it "returns True" $ property $ do
        (b, pb) <- genConsistentPartialBoard
        return $ isConsistent pb b `shouldBe` True
    context "given a PartialBoard with some of the flipped Tiles not equal to the corresponding Tiles in the given Board" $
      it "returns False" $ property $ do
        (b, pb) <- genInconsistentPartialBoard
        return $ isConsistent pb b `shouldBe` False

  describe "isComplete" $ do
    context "given a PartialBoard with all of the non-trivial Tiles flipped" $
      it "returns True" $ property $ do
        (b, pb) <- genCompletePartialBoard
        return $ isComplete pb b `shouldBe` True
    context "given a PartialBoard with a non-trivial Tile unflipped" $
      it "returns False" $ property $ do
        (b, pb) <- genIncompletePartialBoard
        return $ isComplete pb b `shouldBe` False
