module PartialBoardSpec
  ( spec
  , completePartialBoard
  , consistentPartialBoard
  , incompletePartialBoard
  , inconsistentPartialBoard
  ) where

import Control.Exception (evaluate)
import Data.Function (on)
import Data.List (nubBy)
import Test.Hspec
import Test.QuickCheck

import Board (Board, cluesFor, findOptionalTiles, findRequiredTiles, tileAt, tilesAt)
import PartialBoard
import Position (positionsByColumn)

import ArrayGenerators (completeBoundedArray, incompleteBoundedArray, distinctAssocsTuple)
import BoardSpec ()
import CluesSpec ()
import PositionSpec ()
import TileSpec ()

instance Arbitrary PartialBoard where
  arbitrary = partialBoard <$> completeBoundedArray

consistentPartialBoard :: Gen (Board, PartialBoard)
consistentPartialBoard = do
  b  <- arbitrary
  pb <- fmap (flipTilesAtWith emptyBoard b) $ sublistOf positionsByColumn
  return (b, pb)

inconsistentPartialBoard :: Gen (Board, PartialBoard)
inconsistentPartialBoard = do
  b   <- arbitrary
  p   <- arbitrary
  t   <- arbitrary `suchThat` (tileAt b p /=)
  pb  <- fmap (\pb -> updateMaybeTileAt pb p $ Just t) arbitrary
  return (b, pb)

completePartialBoard :: Gen (Board, PartialBoard)
completePartialBoard = do
  b  <- arbitrary
  ps <- (findRequiredTiles b ++) <$> sublistOf (findOptionalTiles b)
  let pb = flipTilesAtWith emptyBoard b ps
  return (b, pb)

incompletePartialBoard :: Gen (Board, PartialBoard)
incompletePartialBoard = do
  b  <- arbitrary `suchThat` (not . null . findRequiredTiles)
  ps <- let ns = findRequiredTiles b in
    (++) <$> sublistOf (findOptionalTiles b) <*> sublistOf ns `suchThat` (/=) ns
  let pb = flipTilesAtWith emptyBoard b ps
  return (b, pb)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

spec :: Spec
spec = do
  describe "partialBoard" $ do
    context "given an Array with incomplete bounds" $ do
      it "is inverted by unPartialBoard" $ property $
        forAll (completeBoundedArray) $ \a ->
          unPartialBoard (partialBoard a) `shouldBe` a
    context "given an Array with complete bounds" $ do
      it "returns an error" $ property $
        forAll (incompleteBoundedArray) $ \arr ->
          evaluate (partialBoard arr) `shouldThrow` errorCall "Array does not have complete bounds"

  describe "emptyBoard" $
    it "contains Nothing" $ property $
      \p -> maybeTileAt emptyBoard p `shouldBe` Nothing

  describe "maybeTileAt" $
    context "getting the Maybe Tile at a Position of a PartialBoard that was just updated" $ do
      it "returns the Maybe Tile that was just updated" $ property $
        \pb p mt -> maybeTileAt (updateMaybeTileAt pb p mt) p `shouldBe` mt

  describe "updateMaybeTileAt" $ do
    it "returns a valid PartialBoard" $ property $
      \pb p mt -> updateMaybeTileAt pb p mt `shouldSatisfy` isValidPartialBoard
    context "updating the Maybe Tile at a Position of a PartialBoard with the Maybe Tile at that Position" $ do
      it "returns the original PartialBoard" $ property $
        \pb p -> updateMaybeTileAt pb p (maybeTileAt pb p) `shouldBe` pb
    context "updating the Maybe Tile at a Position of a PartialBoard twice" $ do
      it "returns the same PartialBoard that results from only updating the PartialBoard the second time" $ property $
        \pb p mt mt' -> updateMaybeTileAt (updateMaybeTileAt pb p mt) p mt' `shouldBe` updateMaybeTileAt pb p mt'

  describe "maybeTilesAt" $
    context "getting the Maybe Tiles at some Positions of a PartialBoard that were just updated" $ do
      it "returns the Tiles that were updated" $ property $
        forAll (arbitrary) $ \pb ->
        forAll (distinctAssocsTuple) $ \(ps, mts) ->
          maybeTilesAt (updateMaybeTilesAt pb $ ps `zip` mts) ps `shouldBe` mts

  describe "updateMaybeTilesAt" $ do
    it "returns a valid PartialBoard" $ property $
      \pb as -> updateMaybeTilesAt pb as `shouldSatisfy` isValidPartialBoard
    context "updating the Maybe Tiles at some Positions of a PartialBoard with the Maybe Tiles at those Positions" $ do
      it "returns the original PartialBoard" $ property $
        \pb ps -> updateMaybeTilesAt pb (ps `zip` maybeTilesAt pb ps) `shouldBe` pb
    context "updating some Maybe Tiles at some Positions of a PartialBoard twice" $ do
      it "returns the same result PartialBoard that results from only updating the PartialBoard a second time" $ property $
        forAll (arbitrary) $ \pb ->
        forAll (fmap (unzip3 . nubBy ((==) `on` fst3)) arbitrary) $ \(ps, mts, mts') ->
          updateMaybeTilesAt (updateMaybeTilesAt pb $ ps `zip` mts) (ps `zip` mts') `shouldBe` updateMaybeTilesAt pb (ps `zip` mts')

  describe "flipTileAtWith" $ do
    it "returns a valid PartialBoard" $ property $
      \pb b p -> flipTileAtWith pb b p `shouldSatisfy` isValidPartialBoard
    it "flips the Tile at the given Position in a Board" $ property  $ do
      \pb b p -> maybeTileAt (flipTileAtWith b p pb) p `shouldBe` Just (tileAt b p)
    it "otherwise leaves the PartialBoard unchanged" $ property $
      \pb b p p' -> p /= p' ==> maybeTileAt (flipTileAtWith b p pb) p' `shouldBe` maybeTileAt pb p'
    it "preserves the consistency of a PartialBoard" $ property $
      forAll (consistentPartialBoard) $ \(b, pb) ->
      forAll (arbitrary) $ \p ->
        flipTileAtWith b p pb `shouldSatisfy` flip isConsistentWith b

  describe "flipTilesAtWith" $ do
    it "returns a valid PartialBoard" $ property $
      \pb b ps -> flipTilesAtWith pb b ps `shouldSatisfy` isValidPartialBoard
    it "flips the Tiles at the given Positions in a Board" $ property $
      \pb b ps -> maybeTilesAt (flipTilesAtWith pb b ps) ps `shouldBe` map Just (tilesAt b ps)
    it "otherwise leaves the PartialBoard unchanged" $ property $
      \pb b ps p -> not (p `elem` ps) ==> maybeTileAt (flipTilesAtWith pb b ps) p `shouldBe` maybeTileAt pb p
    it "preserves the consistency of a PartialBoard" $ property $
      forAll (consistentPartialBoard) $ \(b, pb) ->
      forAll (arbitrary) $ \ps ->
        flipTilesAtWith pb b ps `shouldSatisfy` flip isConsistentWith b

  describe "isConsistentWith" $ do
    context "given an empty PartialBoard" $
      it "returns True" $ property $
        \b -> isConsistentWith emptyBoard b `shouldBe` True
    context "given a PartialBoard with all of the flipped Tiles equal to the corresponding Tiles in the given Board" $
      it "returns True" $ property $
        forAll (consistentPartialBoard) $ \(b, pb) ->
          isConsistentWith pb b `shouldBe` True
    context "given a PartialBoard with some of the flipped Tiles not equal to the corresponding Tiles in the given Board" $
      it "returns False" $ property $
        forAll (inconsistentPartialBoard) $ \(b, pb) ->
          isConsistentWith pb b `shouldBe` False

  describe "isCompleteWith" $ do
    context "given a PartialBoard with all of the non-trivial Tiles flipped" $
      it "returns True" $ property $
        forAll (completePartialBoard) $ \(b, pb) ->
          isCompleteWith pb b `shouldBe` True
    context "given a PartialBoard with a non-trivial Tile unflipped" $
      it "returns False" $ property $
        forAll (incompletePartialBoard) $ \(b, pb) ->
          isCompleteWith pb b `shouldBe` False

  describe "isConsistentWithClues" $ do
    context "given an empty PartialBoard" $
      it "returns True" $ property $
        \cs -> isConsistentWithClues emptyBoard cs `shouldBe` True
    context "given a PartialBoard with all of the flipped Tiles equal to the corresponding Tiles in the given Board and Clues" $
      it "returns True" $ property $
        forAll (consistentPartialBoard) $ \(b, pb) ->
        let cs = cluesFor b in
          isConsistentWithClues pb cs `shouldBe` True

  describe "isCompleteWithClues" $ do
    context "given a PartialBoard with all of the non-trivial Tiles flipped" $
      it "returns True" $ property $
        forAll (completePartialBoard) $ \(b, pb) ->
        let cs = cluesFor b in
          isCompleteWithClues pb cs `shouldBe` True
    context "given a PartialBoard with a non-trivial Tile unflipped" $
      it "returns False" $ property $
        forAll (incompletePartialBoard) $ \(b, pb) ->
        let cs = cluesFor b in
          isCompleteWithClues pb cs `shouldBe` False
