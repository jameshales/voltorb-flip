module BoardSpec (spec) where

import Data.Array ((//))
import Data.Function (on)
import Data.List (nubBy, (\\))
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances (genTileArray)
import Board
import Position (column, positionsByColumn, row)
import Tile (isNonTrivial, numberOfVoltorbs, sumOfTiles)

spec :: Spec
spec = do
  describe "board" $ do
    it "is inverted by unBoard" $ property $ do
      a <- genTileArray
      return $ unBoard (board a) `shouldBe` a

  describe "findNonTrivialTiles" $ do
    it "returns a list of Positions such that every Position in the list contains a non-trivial Tile" $ property $ do
      \b -> findNonTrivialTiles b `shouldSatisfy` all (isNonTrivial . tileAt b)
    it "returns a list of Positions such that every Position not in the list contains a trivial Tile" $ property $ do
      \b -> positionsByColumn \\ findNonTrivialTiles b `shouldSatisfy` all (not . isNonTrivial . tileAt b)

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
