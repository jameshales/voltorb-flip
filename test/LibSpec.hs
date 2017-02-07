module LibSpec (spec) where

import Control.Exception (evaluate)
import Data.Array (assocs, (//))
import Data.List (nub, nubBy, sort, sortBy)
import Data.Function (on)
import Data.Ord (comparing)
import Test.Hspec
import Test.QuickCheck

import Lib

instance Arbitrary Tile where
  arbitrary = elements tiles

instance Arbitrary Coordinate where
  arbitrary = elements coordinates

instance Arbitrary Position where
  arbitrary = elements positionsByRow

instance Arbitrary Board where
  arbitrary = do
    tiles <- infiniteListOf arbitrary
    return $ board $ zip positionsByRow tiles

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

  describe "instance Enum Coordinate" $ do
    describe "succ" $ do
      it "returns an error on maxBound" $ do
        evaluate (succ (maxBound :: Coordinate)) `shouldThrow` anyException
    describe "pred" $ do
      it "returns an error on minBound" $ do
        evaluate (pred (minBound :: Coordinate)) `shouldThrow` anyException
    describe "toEnum" $ do
      context "when the value is less than 0" $ do
        it "returns an error" $ property $ do
          i <- choose (minBound, 0)
          return $ evaluate (toEnum i :: Coordinate) `shouldThrow` anyException
      context "when the value is greater than 4" $ do
        it "returns an error" $ property $ do
          i <- choose (5, maxBound)
          return $ evaluate (toEnum i :: Coordinate) `shouldThrow` anyException
      context "otherwise" $ do
        it "is inverted by fromEnum" $ property $ do
          i <- choose (0, 4)
          return $ (fromEnum $ (toEnum i :: Coordinate)) `shouldBe` i
    describe "fromEnum" $ do
      it "is inverted by toEnum" $ property $
        \c -> (toEnum $ fromEnum c :: Coordinate) `shouldBe` c

  describe "instance Enum Position" $ do
    describe "succ" $ do
      it "returns an error on maxBound" $ do
        evaluate (succ maxBound :: Position) `shouldThrow` anyException
    describe "pred" $ do
      it "returns an error on minBound" $ do
        evaluate (pred minBound :: Position) `shouldThrow` anyException
    describe "toEnum" $ do
      context "when the value is less than 0" $ do
        it "returns an error" $ property $ do
          i <- choose (minBound, 0)
          return $ evaluate (toEnum i :: Position) `shouldThrow` anyException
      context "when the value is greater than 24" $ do
        it "returns an error" $ property $ do
          i <- choose (25, maxBound)
          return $ evaluate (toEnum i :: Position) `shouldThrow` anyException
      context "otherwise" $ do
        it "is inverted by fromEnum" $ property $ do
          i <- choose (0, 24)
          return $ (fromEnum $ (toEnum i :: Position)) `shouldBe` i
    describe "fromEnum" $ do
      it "is inverted by toEnum" $ property $
        \p -> (toEnum $ fromEnum p :: Position) `shouldBe` p

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

  describe "coordinate" $ do
    context "when the value is less than 0" $ do
      it "returns an error" $ property $ do
        i <- choose (minBound, 0)
        return $ evaluate (coordinate i) `shouldThrow` anyException
    context "when the value is greater than 4" $ do
      it "returns an error" $ property $ do
        i <- choose (5, maxBound)
        return $ evaluate (coordinate i) `shouldThrow` anyException
    context "otherwise" $ do
      it "is inverted by unCoordinate" $ property $ do
        i <- choose (0, 4)
        return $ unCoordinate (coordinate i) `shouldBe` i

  describe "coordinates" $ do
    it "returns a list of 5 coordinates" $ do
      length coordinates `shouldBe` 5
    it "returns a list of distinct coordinates" $ do
      nub coordinates `shouldBe` coordinates
    it "returns a list of coordinates in sorted order" $ do
      sort coordinates `shouldBe` coordinates

  describe "position" $ do
    it "returns a position with the given x coordinate" $ property $
      \x y -> columnOf (position x y) `shouldBe` x
    it "returns a position with the given y coordinate" $ property $
      \x y -> rowOf (position x y) `shouldBe` y
    it "is inverted by unPosition" $ property $
      \x y -> unPosition (position x y) `shouldBe` (x, y)

  describe "row" $ do
    it "returns a list of 5 positions" $ property $
      \y -> length (row y) `shouldBe` 5
    it "returns a list of positions in sorted order" $ property $
      \y -> sort (row y) `shouldBe` row y
    it "returns a list of positions with the given y coordinate" $ property $
      \y -> row y `shouldSatisfy` (all (\p -> rowOf p == y))

  describe "column" $ do
    it "returns a list of 5 positions" $ property $
      \x -> length (column x) `shouldBe` 5
    it "returns a list of positions in sorted order" $ property $
      \x -> sort (column x) `shouldBe` column x
    it "returns a list of positions with the given x coordinate" $ property $
      \x -> column x `shouldSatisfy` (all (\p -> columnOf p == x))

  describe "rows" $ do
    it "returns a list of 5 lists of positions" $ do
      length rows `shouldBe` 5
    it "returns a list of lists of 5 positions" $ do
      rows `shouldSatisfy` all (\r -> length r == 5)
    it "returns a list of lists of positions sorted by y coordinate" $ do
      let sortedRows = map (rowOf . head) rows
      sort sortedRows `shouldBe` sortedRows
    it "returns a list of rows" $ do
      map (row . rowOf . head) rows `shouldBe` rows

  describe "columns" $ do
    it "returns a list of 5 lists of positions" $ do
      length columns `shouldBe` 5
    it "returns a list of lists of 5 positions" $ do
      columns `shouldSatisfy` all (\r -> length r == 5)
    it "returns a list of lists of positions sorted by y coordinate" $ do
      let sortedColumns = map (columnOf . head) columns
      sort sortedColumns `shouldBe` sortedColumns
    it "returns a list of columns" $ do
      map (column . columnOf . head) columns `shouldBe` columns

  describe "positionsByRow" $ do
    it "returns a list of 25 positions" $ do
      length positionsByRow `shouldBe` 25
    it "returns a list of distinct positions" $ do
      nub positionsByRow `shouldBe` positionsByRow
    it "returns a list of positions sorted by row then by column" $ do
      sortBy (comparing (\p -> (rowOf p, columnOf p))) positionsByRow `shouldBe` positionsByRow

  describe "positionsByColumn" $ do
    it "returns a list of 25 positions" $ do
      length positionsByColumn `shouldBe` 25
    it "returns a list of distinct positions" $ do
      nub positionsByColumn `shouldBe` positionsByColumn
    it "returns a list of positions sorted by column then by row" $ do
      sortBy (comparing (\p -> (columnOf p, rowOf p))) positionsByColumn `shouldBe` positionsByColumn

  describe "sumOfTiles" $ do
    it "returns 0 for an empty list" $ do
      sumOfTiles [] `shouldBe` 0
    it "increments the sum by the value of a tile as the tile is consed to the given list" $ property $
      \t ts -> sumOfTiles (t:ts) `shouldBe` (unTile t + sumOfTiles ts)

  describe "sumOfTilesAt" $ 
    it "returns the sumOfTiles in the tiles at the given list of positions" $ property $ do
      b      <- arbitrary
      as     <- listOf arbitrary
      let as' = nubBy ((==) `on` fst) as
      let ps  = map fst as'
      let ts  = map snd as'
      let b'  = board $ assocs $ (unBoard b) // as'
      return $ sumOfTilesAt b' ps `shouldBe` sumOfTiles ts

  describe "sumOfTilesAtRow" $
    it "returns the number of Voltorbs in the tiles in the given row" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = board $ assocs $ (unBoard b) // (row c `zip` ts)
      return $ sumOfTilesAtRow b' c `shouldBe` sumOfTiles ts

  describe "sumOfTilesAtColumn" $
    it "returns the number of Voltorbs in the tiles in the given column" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = board $ assocs $ (unBoard b) // (column c `zip` ts)
      return $ sumOfTilesAtColumn b' c `shouldBe` sumOfTiles ts

  describe "numberOfVoltorbs" $ do
    it "returns 0 for an empty list" $ do
      numberOfVoltorbs [] `shouldBe` 0
    it "returns the same count if a non-Voltorb tile is consed to the given list" $ property $
      \t ts -> t /= voltorb ==> numberOfVoltorbs (t:ts) `shouldBe` numberOfVoltorbs ts
    it "increments the count of a Voltorb tile is consed to the given list" $ property $
      \ts -> numberOfVoltorbs (voltorb:ts) `shouldBe` numberOfVoltorbs ts + 1

  describe "numberOfVoltorbsAt" $ 
    it "returns the numberOfVoltorbs in the tiles at the given list of positions" $ property $ do
      b      <- arbitrary
      as     <- listOf arbitrary
      let as' = nubBy ((==) `on` fst) as
      let ps  = map fst as'
      let ts  = map snd as'
      let b'  = board $ assocs $ (unBoard b) // as'
      return $ numberOfVoltorbsAt b' ps `shouldBe` numberOfVoltorbs ts

  describe "numberOfVoltorbsAtRow" $
    it "returns the number of Voltorbs in the tiles in the given row" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = board $ assocs $ (unBoard b) // (row c `zip` ts)
      return $ numberOfVoltorbsAtRow b' c `shouldBe` numberOfVoltorbs ts

  describe "numberOfVoltorbsAtColumn" $
    it "returns the number of Voltorbs in the tiles in the given column" $ property $ do
      b     <- arbitrary
      c     <- arbitrary
      ts    <- vectorOf 5 arbitrary
      let b' = board $ assocs $ (unBoard b) // (column c `zip` ts)
      return $ numberOfVoltorbsAtColumn b' c `shouldBe` numberOfVoltorbs ts
