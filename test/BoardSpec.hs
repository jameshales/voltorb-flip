module BoardSpec (spec) where

import Control.Exception (evaluate)
import Data.Function (on)
import Data.List (nubBy, (\\))
import Test.Hspec
import Test.QuickCheck

import Board
import Clues (clueAt)
import Position (axis, positionsByColumn)
import Tile (clueFor, isVoltorb, isOptional, isRequired)

import ArrayGenerators (completeBoundedArray, incompleteBoundedArray, distinctAssocsTuple)
import TileSpec ()
import PositionSpec ()

instance Arbitrary Board where
  arbitrary = board <$> completeBoundedArray

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

spec :: Spec
spec = do
  describe "board" $ do
    context "given an Array with complete bounds" $ do
      it "is inverted by unBoard" $ property $
        forAll (completeBoundedArray) $ \a ->
          unBoard (board a) `shouldBe` a
    context "given an Array with incomplete bounds" $ do
      it "returns an error" $ property $
        forAll (incompleteBoundedArray) $ \arr ->
          evaluate (board arr) `shouldThrow` errorCall "Array does not have complete bounds"

  describe "tileAt" $ do
    context "getting the Tile at a Position of a Board that was just updated" $ do
      it "returns the Tile that was updated" $ property $
        \b p t -> tileAt (updateTileAt b p t) p `shouldBe` t

  describe "updateTileAt" $ do
    it "returns a valid Board" $ property $
      \b p t -> updateTileAt b p t `shouldSatisfy` isValidBoard
    context "updating the Tile at a Position of a Board with the Tile at that Position" $ do
      it "returns the original Board" $ property $
        \b p -> updateTileAt b p (tileAt b p) `shouldBe` b
    context "updating a Tile at a Position of a Board twice" $ do
      it "returns the same Board that results from only updating the Board the second time" $ property $
        \b p t t' -> updateTileAt (updateTileAt b p t) p t' `shouldBe` updateTileAt b p t'

  describe "tilesAt" $ do
    context "getting the Tiles at some Positions of a Board that were just updated" $ do
      it "returns the Tiles that were updated" $ property $
        forAll (arbitrary) $ \b ->
        forAll (distinctAssocsTuple) $ \(ps, ts) ->
          tilesAt (updateTilesAt b $ ps `zip` ts) ps `shouldBe` ts

  describe "updateTilesAt" $ do
    it "returns a valid Board" $ property $
      \b as -> updateTilesAt b as `shouldSatisfy` isValidBoard
    context "updating the Tiles at some Positions of a Board with the Tiles at those Positions" $ do
      it "returns the original Board" $ property $
        \b ps -> updateTilesAt b (ps `zip` tilesAt b ps) `shouldBe` b
    context "updating some Tiles at some Positions of a Board twice" $ do
      it "returns the same result Board that results from only updating the Board a second time" $ property $
        forAll (arbitrary) $ \b ->
        forAll (fmap (unzip3 . nubBy ((==) `on` fst3)) arbitrary) $ \(ps, ts, ts') ->
          updateTilesAt (updateTilesAt b $ ps `zip` ts) (ps `zip` ts') `shouldBe` updateTilesAt b (ps `zip` ts')

  describe "findVoltorbTiles" $ do
    it "returns a list of Positions such that every Position in the list contains an Voltorb Tile" $ property $
      \b -> findVoltorbTiles b `shouldSatisfy` all (isVoltorb . tileAt b)
    it "returns a list of Positions such that every Position not in the list contains a non-Voltorb Tile" $ property $
      \b -> positionsByColumn \\ findVoltorbTiles b `shouldSatisfy` all (not . isVoltorb . tileAt b)

  describe "findOptionalTiles" $ do
    it "returns a list of Positions such that every Position in the list contains an optional Tile" $ property $
      \b -> findOptionalTiles b `shouldSatisfy` all (isOptional . tileAt b)
    it "returns a list of Positions such that every Position not in the list contains a non-optional Tile" $ property $
      \b -> positionsByColumn \\ findOptionalTiles b `shouldSatisfy` all (not . isOptional . tileAt b)

  describe "findRequiredTiles" $ do
    it "returns a list of Positions such that every Position in the list contains a non-trivial Tile" $ property $
      \b -> findRequiredTiles b `shouldSatisfy` all (isRequired . tileAt b)
    it "returns a list of Positions such that every Position not in the list contains a trivial Tile" $ property $
      \b -> positionsByColumn \\ findRequiredTiles b `shouldSatisfy` all (not . isRequired . tileAt b)

  describe "cluesFor" $ do
    it "returns Clues containing the Clue for each Axis" $ property $
      \b a -> cluesFor b `clueAt` a `shouldBe` (clueFor $ tilesAt b $ axis a)
