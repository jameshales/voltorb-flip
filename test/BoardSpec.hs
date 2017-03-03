module BoardSpec (spec) where

import Control.Exception (evaluate)
import Data.Array (Array, array, listArray)
import Data.Function (on)
import Data.List (nubBy, (\\))
import Test.Hspec
import Test.QuickCheck

import Board
import Clue (Clue(..))
import Clues (clueAt)
import Position (Position, axis, positionsByColumn)
import Tile (Tile, isOptional, isRequired, numberOfVoltorbs, sumOfTiles)

import TileSpec ()
import PositionSpec ()

boardArray :: Gen (Array Position Tile)
boardArray = fmap (array (minBound, maxBound) . zip positionsByColumn) $ infiniteListOf arbitrary

instance Arbitrary Board where
  arbitrary = fmap board boardArray

boardAssocs :: Gen [(Position, Tile)]
boardAssocs = fmap (nubBy ((==) `on` fst)) arbitrary

boardAssocsTuple :: Gen ([Position], [Tile])
boardAssocsTuple = fmap unzip boardAssocs

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

spec :: Spec
spec = do
  describe "board" $ do
    context "given a valid Array of Tiles" $ do
      it "is inverted by unBoard" $ property $ do
        a <- boardArray
        return $ unBoard (board a) `shouldBe` a
    context "given an Array with bounds less than (minBound, maxBound)" $ do
      it "returns an error" $ property $ do
        let invalidBounds = (/= (minBound, maxBound))
        bounds  <- arbitrary `suchThat` invalidBounds
        ts      <- vectorOf 25 arbitrary
        let arr  = listArray bounds ts
        return $ evaluate (board arr) `shouldThrow` errorCall "Array does not have full bounds"

  describe "tileAt" $ do
    context "getting the Tile at a Position of a Board that was just updated" $ do
      it "returns the Tile that was updated" $ property $ do
        \b p t -> tileAt (updateTileAt b p t) p `shouldBe` t

  describe "updateTileAt" $ do
    it "returns a valid Board" $ property $ do
      \b p t -> updateTileAt b p t `shouldSatisfy` isValidBoard
    context "updating the Tile at a Position of a Board with the Tile at that Position" $ do
      it "returns the original Board" $ property $ do
        \b p -> updateTileAt b p (tileAt b p) `shouldBe` b
    context "updating a Tile at a Position of a Board twice" $ do
      it "returns the same Board that results from only updating the Board the second time" $ property $ do
        \b p t t' -> updateTileAt (updateTileAt b p t) p t' `shouldBe` updateTileAt b p t'

  describe "tilesAt" $ do
    context "getting the Tiles at some Positions of a Board that were just updated" $ do
      it "returns the Tiles that were updated" $ property $ do
        b         <- arbitrary
        (ps, ts)  <- boardAssocsTuple
        return $ tilesAt (updateTilesAt b $ ps `zip` ts) ps `shouldBe` ts

  describe "updateTilesAt" $ do
    it "returns a valid Board" $ property $ do
      \b as -> updateTilesAt b as `shouldSatisfy` isValidBoard
    context "updating the Tiles at some Positions of a Board with the Tiles at those Positions" $ do
      it "returns the original Board" $ property $ do
        \b ps -> updateTilesAt b (ps `zip` tilesAt b ps) `shouldBe` b
    context "updating some Tiles at some Positions of a Board twice" $ do
      it "returns the same result Board that results from only updating the Board a second time" $ property $ do
        b             <- arbitrary
        (ps, ts, ts') <- fmap (unzip3 . nubBy ((==) `on` fst3)) arbitrary
        return $ updateTilesAt (updateTilesAt b $ ps `zip` ts) (ps `zip` ts') `shouldBe` updateTilesAt b (ps `zip` ts')

  describe "findOptionalTiles" $ do
    it "returns a list of Positions such that every Position in the list contains an optional Tile" $ property $ do
      \b -> findOptionalTiles b `shouldSatisfy` all (isOptional . tileAt b)
    it "returns a list of Positions such that every Position not in the list contains a non-optional Tile" $ property $ do
      \b -> positionsByColumn \\ findOptionalTiles b `shouldSatisfy` all (not . isOptional . tileAt b)

  describe "findRequiredTiles" $ do
    it "returns a list of Positions such that every Position in the list contains a non-trivial Tile" $ property $ do
      \b -> findRequiredTiles b `shouldSatisfy` all (isRequired . tileAt b)
    it "returns a list of Positions such that every Position not in the list contains a trivial Tile" $ property $ do
      \b -> positionsByColumn \\ findRequiredTiles b `shouldSatisfy` all (not . isRequired . tileAt b)

  describe "sumOfTilesAt" $
    it "returns the sumOfTiles in the tiles at the given list of positions" $ property $ do
      b         <- arbitrary
      (ps, ts)  <- boardAssocsTuple
      let b'     = updateTilesAt b $ ps `zip` ts
      return $ sumOfTilesAt b' ps `shouldBe` sumOfTiles ts

  describe "sumOfTilesAtAxis" $
    it "returns the number of Voltorbs in the tiles in the given Axis" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = updateTilesAt b $ axis c `zip` ts
      return $ sumOfTilesAtAxis b' c `shouldBe` sumOfTiles ts

  describe "numberOfVoltorbsAt" $
    it "returns the numberOfVoltorbs in the tiles at the given list of positions" $ property $ do
      b         <- arbitrary
      (ps, ts)  <- boardAssocsTuple
      let b'  = updateTilesAt b $ ps `zip` ts
      return $ numberOfVoltorbsAt b' ps `shouldBe` numberOfVoltorbs ts

  describe "numberOfVoltorbsAtAxis" $
    it "returns the number of Voltorbs in the Tiles in the given Axis" $ property $ do
      b     <- arbitrary
      a     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = updateTilesAt b $ axis a `zip` ts
      return $ numberOfVoltorbsAtAxis b' a `shouldBe` numberOfVoltorbs ts

  describe "clueAtAxis" $ do
    it "returns a Clue with the sum of Tiles at the given Axis of the Board" $ property $ do
      \b a -> getSumOfTiles (clueAtAxis b a) `shouldBe` sumOfTilesAtAxis b a
    it "returns a Clue with the number of Voltorbs at the given Axis of the Board" $ property $ do
      \b a -> getNumberOfVoltorbs (clueAtAxis b a) `shouldBe` numberOfVoltorbsAtAxis b a

  describe "cluesFor" $ do
    it "returns Clues containing the Clue for each Axis" $ property $ do
      \b a -> cluesFor b `clueAt` a `shouldBe` clueAtAxis b a
