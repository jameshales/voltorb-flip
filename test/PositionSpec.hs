module PositionSpec (spec) where

import Control.Exception (ErrorCall, evaluate)
import Data.List (nub, sort, sortBy)
import Data.Ord (comparing)
import Test.Hspec
import Test.QuickCheck

import Axis (Axis(Column, Row))
import Position

import CoordinateSpec ()
import AxisSpec ()

instance Arbitrary Position where
  arbitrary = elements positionsByColumn

positionBeforeRowEnd :: Gen Position
positionBeforeRowEnd = position <$> arbitrary `suchThat` (/=) maxBound <*> arbitrary

positionAtRowEnd :: Gen Position
positionAtRowEnd = position maxBound <$> arbitrary `suchThat` (/=) maxBound

positionAfterRowStart :: Gen Position
positionAfterRowStart = position <$> arbitrary `suchThat` (/=) minBound <*> arbitrary

positionAtRowStart :: Gen Position
positionAtRowStart = position minBound <$> arbitrary `suchThat` (/= minBound)

outOfBoundsError :: Selector ErrorCall
outOfBoundsError = errorCall "Position out of bounds"

spec :: Spec
spec = do
  describe "instance Enum Position" $ do
    describe "succ" $ do
      context "given a Position before the end of a row" $ do
        it "increments the X coordinate" $ property $
          forAll (positionBeforeRowEnd) $ \p ->
            getX (succ p) `shouldBe` succ (getX p)
        it "keeps the Y coordinate the same" $ property $
          forAll (positionBeforeRowEnd) $ \p ->
            getY (succ p) `shouldBe` getY p
      context "given a Position at the end of a row (but not maxBound)" $ do
        it "increments the Y coordinate" $ property $
          forAll (positionAtRowEnd) $ \p ->
            getY (succ p) `shouldBe` succ (getY p)
        it "resets the X coordinate to 0" $ property $
          forAll (positionAtRowEnd) $ \p ->
            getX (succ p) `shouldBe` minBound
      context "given maxBound" $ do
        it "returns an error" $ do
          evaluate (succ maxBound :: Position) `shouldThrow` outOfBoundsError
    describe "pred" $ do
      context "given a Position after the start of a row" $ do
        it "decrements the X coordinate" $ property $
          forAll (positionAfterRowStart) $ \p ->
            getX (pred p) `shouldBe` pred (getX p)
        it "keeps the Y coordinate the same" $ property $
          forAll (positionAfterRowStart) $ \p ->
            getY (pred p) `shouldBe` getY p
      context "given a Position at the start of a row (but not minBound)" $ do
        it "decrements the Y coordinate" $ property $
          forAll (positionAtRowStart) $ \p ->
            getY (pred p) `shouldBe` pred (getY p)
        it "resets the X coordinate to maxBound" $ property $
          forAll (positionAtRowStart) $ \p ->
            getX (pred p) `shouldBe` maxBound
      context "given minBound" $ do
        it "returns an error" $ do
          evaluate (pred minBound :: Position) `shouldThrow` outOfBoundsError
    describe "toEnum" $ do
      context "when the value is less than 0" $ do
        it "returns an error" $ property $
          forAll (choose (minBound, -1)) $ \i ->
            evaluate (toEnum i :: Position) `shouldThrow` outOfBoundsError
      context "when the value is greater than 24" $ do
        it "returns an error" $ property $
          forAll (choose (25, maxBound)) $ \i ->
            evaluate (toEnum i :: Position) `shouldThrow` outOfBoundsError
      context "otherwise" $ do
        it "is inverted by fromEnum" $ property $
          forAll (choose (0, 24)) $ \i ->
            (fromEnum $ (toEnum i :: Position)) `shouldBe` i
    describe "fromEnum" $ do
      it "is inverted by toEnum" $ property $
        \p -> (toEnum $ fromEnum p :: Position) `shouldBe` p

  describe "position" $ do
    it "returns a Position with the given x coordinate" $ property $
      \x y -> getX (position x y) `shouldBe` x
    it "returns a Position with the given y coordinate" $ property $
      \x y -> getY (position x y) `shouldBe` y
    it "is inverted by unPosition" $ property $
      \x y -> unPosition (position x y) `shouldBe` (x, y)

  describe "row" $ do
    it "returns a list of 5 Positions" $ property $
      \y -> length (row y) `shouldBe` 5
    it "returns a list of Positions in sorted order" $ property $
      \y -> sort (row y) `shouldBe` row y
    it "returns a list of Positions with the given y coordinate" $ property $
      \y -> row y `shouldSatisfy` (all (\p -> getY p == y))
    it "returns a list of distinct Positions" $ property $
      \x -> nub (row x) `shouldBe` row x

  describe "column" $ do
    it "returns a list of 5 Positions" $ property $
      \x -> length (column x) `shouldBe` 5
    it "returns a list of Positions in sorted order" $ property $
      \x -> sort (column x) `shouldBe` column x
    it "returns a list of Positions with the given x coordinate" $ property $
      \x -> column x `shouldSatisfy` (all (\p -> getX p == x))
    it "returns a list of distinct Positions" $ property $
      \x -> nub (column x) `shouldBe` column x

  describe "rows" $ do
    it "returns a list of 5 lists of Positions" $ do
      length rows `shouldBe` 5
    it "returns a list of lists of 5 Positions" $ do
      rows `shouldSatisfy` all (\r -> length r == 5)
    it "returns a list of lists of Positions sorted by Y coordinate" $ do
      let ys = map (getY . head) rows
      sort ys `shouldBe` ys
    it "returns a list of lists of Positions with distinct Y coordinate" $ do
      let ys = map (getY . head) rows
      nub ys `shouldBe` ys
    it "returns a list of rows" $ do
      map (row . getY . head) rows `shouldBe` rows

  describe "columns" $ do
    it "returns a list of 5 lists of Positions" $ do
      length columns `shouldBe` 5
    it "returns a list of lists of 5 Positions" $ do
      columns `shouldSatisfy` all (\r -> length r == 5)
    it "returns a list of lists of Positions sorted by Y coordinate" $ do
      let xs = map (getX . head) columns
      sort xs `shouldBe` xs
    it "returns a list of lists of Positions with distinct Y coordinate" $ do
      let xs = map (getX . head) columns
      nub xs `shouldBe` xs
    it "returns a list of columns" $ do
      map (column . getX . head) columns `shouldBe` columns

  describe "positionsByRow" $ do
    it "returns a list of 25 Positions" $ do
      length positionsByRow `shouldBe` 25
    it "returns a list of distinct Positions" $ do
      nub positionsByRow `shouldBe` positionsByRow
    it "returns a list of Positions sorted by row then by column" $ do
      sortBy (comparing (\p -> (getY p, getX p))) positionsByRow `shouldBe` positionsByRow

  describe "positionsByColumn" $ do
    it "returns a list of 25 Positions" $ do
      length positionsByColumn `shouldBe` 25
    it "returns a list of distinct Positions" $ do
      nub positionsByColumn `shouldBe` positionsByColumn
    it "returns a list of Positions sorted by column then by row" $ do
      sortBy (comparing (\p -> (getX p, getY p))) positionsByColumn `shouldBe` positionsByColumn

  describe "axis" $ do
    it "returns a list of 5 columns" $ property $
      \a -> length (axis a) `shouldBe` 5
    it "returns a list of Positions in sorted order" $ property $
      \a -> sort (axis a) `shouldBe` axis a
    it "returns a list of distinct Positions" $ property $
      \a -> nub (axis a) `shouldBe` axis a
    context "given (Column c)" $ do
      it "returns (column c)" $ property $
        forAll (arbitrary) $ \c ->
        let a = Column c in
          axis a `shouldBe` column c
    context "given (Row c)" $ do
      it "returns (row c)" $ property $
        forAll (arbitrary) $ \c ->
        let a = Row c in
          axis a `shouldBe` row c
