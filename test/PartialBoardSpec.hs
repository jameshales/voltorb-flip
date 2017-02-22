module PartialBoardSpec (spec) where

import Control.Exception (evaluate)
import Data.Array (listArray)
import Data.Function (on)
import Data.List (nubBy)
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances
import Board (tileAt, tilesAt)
import PartialBoard
import Position (Position)
import Tile (Tile)

genAssocs :: Gen [(Position, Maybe Tile)]
genAssocs = fmap (nubBy ((==) `on` fst)) arbitrary

genAssocsTuple :: Gen ([Position], [Maybe Tile])
genAssocsTuple = fmap unzip genAssocs

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

spec :: Spec
spec = do
  describe "partialBoard" $ do
    context "given a valid Array of Maybe Tiles" $ do
      it "is inverted by unPartialBoard" $ property $ do
        a <- genMaybeTileArray
        return $ unPartialBoard (partialBoard a) `shouldBe` a
    context "given an Array with bounds less than (minBound, maxBound)" $ do
      it "returns an error" $ property $ do
        let invalidBounds = (/=) (minBound, maxBound)
        arr <- listArray <$> arbitrary `suchThat` invalidBounds <*> infiniteListOf arbitrary
        return $ evaluate (partialBoard arr) `shouldThrow` errorCall "Array does not have full bounds"

  describe "emptyBoard" $
    it "contains Nothing" $ property $ do
      \p -> maybeTileAt emptyBoard p `shouldBe` Nothing

  describe "maybeTileAt" $
    context "getting the Maybe Tile at a Position of a PartialBoard that was just updated" $ do
      it "returns the Maybe Tile that was just updated" $ property $ do
        \pb p mt -> maybeTileAt (updateMaybeTileAt pb p mt) p `shouldBe` mt

  describe "updateMaybeTileAt" $ do
    context "updating the Maybe Tile at a Position of a PartialBoard with the Maybe Tile at that Position" $ do
      it "returns the original PartialBoard" $ property $ do
        \pb p -> updateMaybeTileAt pb p (maybeTileAt pb p) `shouldBe` pb
    context "updating the Maybe Tile at a Position of a PartialBoard twice" $ do
      it "returns the same PartialBoard that results from only updating the PartialBoard the second time" $ property $ do
        \pb p mt mt' -> updateMaybeTileAt (updateMaybeTileAt pb p mt) p mt' `shouldBe` updateMaybeTileAt pb p mt'

  describe "maybeTilesAt" $
    context "getting the Maybe Tiles at some Positions of a PartialBoard that were just updated" $ do
      it "returns the Tiles that were updated" $ property $ do
        pb <- arbitrary
        (ps, mts) <- genAssocsTuple
        return $ maybeTilesAt (updateMaybeTilesAt pb $ ps `zip` mts) ps `shouldBe` mts

  describe "updateMaybeTilesAt" $ do
    context "updating the Maybe Tiles at some Positions of a PartialBoard with the Maybe Tiles at those Positions" $ do
      it "returns the original PartialBoard" $ property $ do
        \pb ps -> updateMaybeTilesAt pb (ps `zip` maybeTilesAt pb ps) `shouldBe` pb
    context "updating some Maybe Tiles at some Positions of a PartialBoard twice" $ do
      it "returns the same result PartialBoard that results from only updating the PartialBoard a second time" $ property $ do
        pb              <- arbitrary
        (ps, mts, mts') <- fmap (unzip3 . nubBy ((==) `on` fst3)) arbitrary
        return $ updateMaybeTilesAt (updateMaybeTilesAt pb $ ps `zip` mts) (ps `zip` mts') `shouldBe` updateMaybeTilesAt pb (ps `zip` mts')

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
