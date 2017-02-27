module PositionSpec (spec) where

import Control.Exception (ErrorCall, evaluate)
import Data.List (nub, sort, sortBy)
import Data.Ord (comparing)
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances ()
import Axis (Axis(Column, Row))
import Position

outOfBoundsError :: Selector ErrorCall
outOfBoundsError = errorCall "Position out of bounds"

spec :: Spec
spec = do
  describe "instance Enum Position" $ do
    describe "succ" $ do
      it "returns an error on maxBound" $ do
        evaluate (succ maxBound :: Position) `shouldThrow` outOfBoundsError
    describe "pred" $ do
      it "returns an error on minBound" $ do
        evaluate (pred minBound :: Position) `shouldThrow` outOfBoundsError
    describe "toEnum" $ do
      context "when the value is less than 0" $ do
        it "returns an error" $ property $ do
          i <- choose (minBound, 0)
          return $ evaluate (toEnum i :: Position) `shouldThrow` outOfBoundsError
      context "when the value is greater than 24" $ do
        it "returns an error" $ property $ do
          i <- choose (25, maxBound)
          return $ evaluate (toEnum i :: Position) `shouldThrow` outOfBoundsError
      context "otherwise" $ do
        it "is inverted by fromEnum" $ property $ do
          i <- choose (0, 24)
          return $ (fromEnum $ (toEnum i :: Position)) `shouldBe` i
    describe "fromEnum" $ do
      it "is inverted by toEnum" $ property $
        \p -> (toEnum $ fromEnum p :: Position) `shouldBe` p

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

  describe "axis" $ do
    it "returns a list of 5 columns" $ property $ do
      \a -> length (axis a) `shouldBe` 5
    it "returns a list of positions in sorted order" $ property $ do
      \a -> sort (axis a) `shouldBe` axis a
    context "given (Column c)" $ do
      it "returns (column c)" $ property $ do
        c <- arbitrary
        let a = Column c
        return $ axis a `shouldBe` column c
    context "given (Row c)" $ do
      it "returns (row c)" $ property $ do
        c <- arbitrary
        let a = Row c
        return $ axis a `shouldBe` row c
