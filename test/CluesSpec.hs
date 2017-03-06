module CluesSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Clues

import ArrayGenerators (completeBoundedArray, incompleteBoundedArray)
import AxisSpec ()
import ClueSpec ()

instance Arbitrary Clues where
  arbitrary = clues <$> completeBoundedArray

spec :: Spec
spec = do
  describe "clues" $ do
    context "given a valid Array of Clues" $ do
      it "is inverted by unClues" $ property $
        forAll (completeBoundedArray) $ \a ->
          unClues (clues a) `shouldBe` a
    context "given an Array with bounds not equal to (minBound, maxBound)" $ do
      it "returns an error" $ property $
        forAll (incompleteBoundedArray) $ \arr ->
          evaluate (clues arr) `shouldThrow` errorCall "Array does not have full bounds"

  describe "clueAt" $ do
    context "getting the Clue for an Axis of some Clues that were just updated" $ do
      it "returns the Clue that was updated" $ property $
        \b p t -> clueAt (updateClueAt b p t) p `shouldBe` t

  describe "updateClueAt" $ do
    it "returns valid Clues" $ property $
      \b p t -> updateClueAt b p t `shouldSatisfy` isValidClues
    context "updating the Clue for an Axis of some Clues with the Clue for that Axis" $ do
      it "returns the original Clues" $ property $
        \b p -> updateClueAt b p (clueAt b p) `shouldBe` b
    context "updating a Clue for an Axis of some Clues twice" $ do
      it "returns the same Clues that result from only updating the Clues the second time" $ property $
        \b p t t' -> updateClueAt (updateClueAt b p t) p t' `shouldBe` updateClueAt b p t'
