module BoardSpec (spec) where

import Control.Exception (evaluate)
import Data.Array (listArray)
import Data.Function (on)
import Data.List (nubBy, (\\))
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances (genTileArray)
import Board
import Position (Position, column, positionsByColumn, row)
import Tile (Tile, isOptional, isRequired, numberOfVoltorbs, sumOfTiles)

genAssocs :: Gen [(Position, Tile)]
genAssocs = fmap (nubBy ((==) `on` fst)) arbitrary

genAssocsTuple :: Gen ([Position], [Tile])
genAssocsTuple = fmap unzip genAssocs

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

spec :: Spec
spec = do
  describe "board" $ do
    context "given a valid Array of Tiles" $ do
      it "is inverted by unBoard" $ property $ do
        a <- genTileArray
        return $ unBoard (board a) `shouldBe` a
    context "given an Array with bounds less than (minBound, maxBound)" $ do
      it "returns an error" $ property $ do
        let invalidBounds (a, b) = a /= minBound || b /= maxBound
        bounds  <- arbitrary `suchThat` invalidBounds
        ts      <- vectorOf 25 arbitrary
        let arr  = listArray bounds ts
        return $ evaluate (board arr) `shouldThrow` errorCall "Array does not have full bounds"

  describe "tileAt" $ do
    context "getting the Tile at a Position of a Board that was just updated" $ do
      it "returns the Tile that was updated" $ property $ do
        \b p t -> tileAt (updateTileAt b p t) p `shouldBe` t

  describe "updateTileAt" $ do
    context "updating the Tile at a Position of a Board with the Tile at that Position" $ do
      it "returns the original Board" $ property $ do
        \b p -> updateTileAt b p (tileAt b p) `shouldBe` b
    context "updating a Tile at a Position of a Board twice" $ do
      it "is the same as only updating the Board the second time" $ property $ do
        \b p t t' -> updateTileAt (updateTileAt b p t) p t' `shouldBe` updateTileAt b p t'

  describe "tilesAt" $ do
    context "getting the Tiles at some Positions of a Board that was just updated" $ do
      it "returns the Tiles that were updated" $ property $ do
        b         <- arbitrary
        (ps, ts)  <- genAssocsTuple
        return $ tilesAt (updateTilesAt b (zip ps ts)) ps `shouldBe` ts

  describe "updateTilesAt" $ do
    context "updating the Tiles at some Positions of a Board with the Tiles at those Positions" $ do
      it "returns the original Board" $ property $ do
        \b ps -> updateTilesAt b (zip ps (tilesAt b ps)) `shouldBe` b
    context "updating some Tiles at some Positions of a Board twice" $ do
      it "is the same as only updating the Board the second time" $ property $ do
        b             <- arbitrary
        (ps, ts, ts') <- fmap (unzip3 . nubBy ((==) `on` fst3)) arbitrary
        return $ updateTilesAt (updateTilesAt b (zip ps ts)) (zip ps ts') `shouldBe` updateTilesAt b (zip ps ts')

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
      (ps, ts)  <- genAssocsTuple
      let b'     = updateTilesAt b $ ps `zip` ts
      return $ sumOfTilesAt b' ps `shouldBe` sumOfTiles ts

  describe "sumOfTilesAtRow" $
    it "returns the number of Voltorbs in the tiles in the given row" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = updateTilesAt b $ row c `zip` ts
      return $ sumOfTilesAtRow b' c `shouldBe` sumOfTiles ts

  describe "sumOfTilesAtColumn" $
    it "returns the number of Voltorbs in the tiles in the given column" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = updateTilesAt b $ column c `zip` ts
      return $ sumOfTilesAtColumn b' c `shouldBe` sumOfTiles ts

  describe "numberOfVoltorbsAt" $
    it "returns the numberOfVoltorbs in the tiles at the given list of positions" $ property $ do
      b         <- arbitrary
      (ps, ts)  <- genAssocsTuple
      let b'  = updateTilesAt b $ ps `zip` ts
      return $ numberOfVoltorbsAt b' ps `shouldBe` numberOfVoltorbs ts

  describe "numberOfVoltorbsAtRow" $
    it "returns the number of Voltorbs in the tiles in the given row" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = updateTilesAt b $ row c `zip` ts
      return $ numberOfVoltorbsAtRow b' c `shouldBe` numberOfVoltorbs ts

  describe "numberOfVoltorbsAtColumn" $
    it "returns the number of Voltorbs in the tiles in the given column" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = updateTilesAt b $ column c `zip` ts
      return $ numberOfVoltorbsAtColumn b' c `shouldBe` numberOfVoltorbs ts
