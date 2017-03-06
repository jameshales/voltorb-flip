module AxisSpec (spec) where

import Control.Exception (ErrorCall, evaluate)
import Data.Ix
import Data.List (nub, sort)
import Test.Hspec
import Test.QuickCheck

import Axis

import CoordinateSpec ()

genColumn :: Gen Axis
genColumn = fmap Column arbitrary

genRow :: Gen Axis
genRow = fmap Row arbitrary

instance Arbitrary Axis where
  arbitrary = oneof [genColumn, genRow]

outOfBoundsError :: Selector ErrorCall
outOfBoundsError = errorCall "Axis out of bounds"

outOfRangeError :: Selector ErrorCall
outOfRangeError = errorCall "Axis out of range"

spec :: Spec
spec = do
  describe "instance Bounded Axis" $ do
    describe "minBound" $ do
      it "is less than or equal to any Axis" $ property $
        \a -> (minBound :: Axis) `shouldSatisfy` (<= a)
    describe "maxBound" $ do
      it "is greater than or equal to any Axis" $ property $
        \a -> (maxBound :: Axis) `shouldSatisfy` (>= a)

  describe "instance Enum Axis" $ do
    describe "succ" $ do
      it "returns an error on maxBound" $ do
        evaluate (succ (maxBound :: Axis)) `shouldThrow` outOfBoundsError
    describe "pred" $ do
      it "returns an error on minBound" $ do
        evaluate (pred (minBound :: Axis)) `shouldThrow` outOfBoundsError
    describe "toEnum" $ do
      context "when the value is less than 0" $ do
        it "returns an error" $ property $
          forAll (choose (minBound, 0)) $ \i ->
            evaluate (toEnum i :: Axis) `shouldThrow` outOfBoundsError
      context "when the value is greater than 9" $
        it "returns an error" $ property $
          forAll (choose (10, maxBound)) $ \i ->
            evaluate (toEnum i :: Axis) `shouldThrow` outOfBoundsError
      context "otherwise" $ do
        it "is inverted by fromEnum" $ property $
          forAll (choose (0, 9)) $ \i ->
            (fromEnum $ (toEnum i :: Axis)) `shouldBe` i
    describe "fromEnum" $ do
      it "is inverted by toEnum" $ property $
        \c -> (toEnum $ fromEnum c :: Axis) `shouldBe` c

  describe "instance Ix Axis" $ do
    it "satisfies inRange (l, u) i == elem i (range (l, u))" $ property $
      \l u i -> inRange (l, u) (i :: Axis) `shouldBe` elem i (range (l, u))
    it "satisfies range (l, u) !! index (l, u) i == i when inRange (l, u) i" $ property $
      \l u i -> inRange (l, u) (i :: Axis) ==> range (l, u) !! index (l, u) i `shouldBe` i
    it "satisfies map (index (l, u)) (range (l, u)) == [0..rangeSize (l, u) - 1]" $ property $
      \l u -> map (index ((l :: Axis), u)) (range (l, u)) `shouldBe` [0..rangeSize (l, u) - 1]
    it "satisfies rangeSize (l, u) == length (range (l, u))" $ property $
      \l u -> rangeSize ((l :: Axis), u) `shouldBe` length (range (l, u))
    describe "index" $ do
      it "returns an error if the index is out of range" $ property $
        \l u i -> not (inRange (l, u) (i :: Axis)) ==> evaluate (index (l, u) i) `shouldThrow` outOfRangeError

  describe "axes" $ do
    it "returns a list of all Axes" $ property $
      \a -> axes `shouldSatisfy` elem a
    it "returns a sorted list of Axes" $ do
      sort axes `shouldBe` axes
    it "returns a distinct list of Axes" $ do
      nub axes `shouldBe` axes

  describe "columnAxes" $ do
    it "returns a list of all Column Axes" $ property $
      \c -> columnAxes `shouldSatisfy` elem (Column c)
    it "returns a list of only Column Axes" $ do
      all (\a -> case a of (Column _) -> True; (Row _) -> False) columnAxes
    it "returns a sorted list of Axes" $ do
      sort columnAxes `shouldBe` columnAxes
    it "returns a distinct list of Axes" $ do
      nub columnAxes `shouldBe` columnAxes

  describe "rowAxes" $ do
    it "returns a list of all Row Axes" $ property $
      \c -> rowAxes `shouldSatisfy` elem (Row c)
    it "returns a list of only Row Axes" $ do
      all (\a -> case a of (Row _) -> True; (Column _) -> False) rowAxes
    it "returns a sorted list of Axes" $ do
      sort rowAxes `shouldBe` rowAxes
    it "returns a distinct list of Axes" $ do
      nub rowAxes `shouldBe` rowAxes
