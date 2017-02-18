module BoardSpec (spec) where

import Control.Exception (evaluate)
import Data.Array (listArray, (//))
import Data.Function (on)
import Data.List (nubBy, (\\))
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances (genTileArray)
import Board
import Position (column, positionsByColumn, row)
import Tile (isOptional, isRequired, numberOfVoltorbs, sumOfTiles)

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
      b      <- arbitrary
      as     <- listOf arbitrary
      let as' = nubBy ((==) `on` fst) as
      let ps  = map fst as'
      let ts  = map snd as'
      let b'  = board $ (unBoard b) // as'
      return $ sumOfTilesAt b' ps `shouldBe` sumOfTiles ts

  describe "sumOfTilesAtRow" $
    it "returns the number of Voltorbs in the tiles in the given row" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = board $ (unBoard b) // (row c `zip` ts)
      return $ sumOfTilesAtRow b' c `shouldBe` sumOfTiles ts

  describe "sumOfTilesAtColumn" $
    it "returns the number of Voltorbs in the tiles in the given column" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = board $ (unBoard b) // (column c `zip` ts)
      return $ sumOfTilesAtColumn b' c `shouldBe` sumOfTiles ts

  describe "numberOfVoltorbsAt" $
    it "returns the numberOfVoltorbs in the tiles at the given list of positions" $ property $ do
      b      <- arbitrary
      as     <- listOf arbitrary
      let as' = nubBy ((==) `on` fst) as
      let ps  = map fst as'
      let ts  = map snd as'
      let b'  = board $ (unBoard b) // as'
      return $ numberOfVoltorbsAt b' ps `shouldBe` numberOfVoltorbs ts

  describe "numberOfVoltorbsAtRow" $
    it "returns the number of Voltorbs in the tiles in the given row" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = board $ (unBoard b) // (row c `zip` ts)
      return $ numberOfVoltorbsAtRow b' c `shouldBe` numberOfVoltorbs ts

  describe "numberOfVoltorbsAtColumn" $
    it "returns the number of Voltorbs in the tiles in the given column" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = board $ (unBoard b) // (column c `zip` ts)
      return $ numberOfVoltorbsAtColumn b' c `shouldBe` numberOfVoltorbs ts
