module PartialBoardSpec (spec) where

import Data.Array ((!), (//))
import Data.Function (on)
import Data.List (nubBy)
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances (genMaybeTileArray)
import Board (findNonTrivialTiles, tileAt, tilesAt)
import PartialBoard

spec :: Spec
spec = do
  describe "partialBoard" $
    it "is inverted by unPartialBoard" $ property $ do
      a <- genMaybeTileArray
      return $ unPartialBoard (partialBoard a) `shouldBe` a

  describe "emptyBoard" $
    it "contains Nothing" $ property $ do
      \p -> (unPartialBoard emptyBoard) ! p `shouldBe` Nothing

  describe "maybeTileAt" $
    it "returns the Maybe Tile at the given Position" $ property $ do
      \pb p mt -> let pb' = partialBoard (unPartialBoard pb // [(p, mt)]) in maybeTileAt pb' p `shouldBe` mt

  describe "maybeTilesAt" $
    it "returns the MaybeTiles at the given Positions" $ property $ do
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

  describe "flipTilesAt" $ do
    it "returns the Tiles at the given Positions in a Board" $ property $ do
      \pb b ps -> fst (flipTilesAt pb b ps) `shouldBe` tilesAt b ps
    it "flips the Tiles at the given Positions in a Board" $ property $ do
      \pb b ps -> maybeTilesAt (snd $ flipTilesAt pb b ps) ps `shouldBe` map Just (tilesAt b ps)
    it "otherwise leaves the PartialBoard unchanged" $ property $ do
      \pb b ps p -> not (p `elem` ps) ==> maybeTileAt (snd $ flipTilesAt pb b ps) p `shouldBe` maybeTileAt pb p 

  describe "isComplete" $ do
    context "given a PartialBoard with all of the non-trivial Tiles flipped" $
      it "returns True" $ property $ do
        \pb b -> let pb' = snd (flipTilesAt pb b (findNonTrivialTiles b)) in isComplete pb' b `shouldBe` True
    context "given a PartialBoard with a non-trivial Tile unflipped" $
      it "returns False" $ property $ do
        b  <- arbitrary `suchThat` (not . null . findNonTrivialTiles)
        ps <- let ns = findNonTrivialTiles b in sublistOf ns `suchThat` (/=) ns
        let pb = snd $ flipTilesAt emptyBoard b ps
        return $ isComplete pb b `shouldBe` False
