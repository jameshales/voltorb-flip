module CluesSpec (spec) where

import Control.Exception (evaluate)
import Data.Array (listArray)
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances
import Clues

spec :: Spec
spec = do
  describe "clues" $ do
    context "given a valid Array of Clues" $ do
      it "is inverted by unClues" $ property $ do
        a <- genClueArray
        return $ unClues (clues a) `shouldBe` a
    context "given an Array with bounds not equal to (minBound, maxBound)" $ do
      it "returns an error" $ property $ do
        let invalidBounds = (/= (minBound, maxBound))
        bounds  <- arbitrary `suchThat` invalidBounds
        ts      <- vectorOf 10 arbitrary
        let arr  = listArray bounds ts
        return $ evaluate (clues arr) `shouldThrow` errorCall "Array does not have full bounds"

  describe "clueAt" $ do
    context "getting the Clue for an Axis of some Clues that were just updated" $ do
      it "returns the Clue that was updated" $ property $ do
        \b p t -> clueAt (updateClueAt b p t) p `shouldBe` t

  describe "updateClueAt" $ do
    it "returns valid Clues" $ property $ do
      \b p t -> updateClueAt b p t `shouldSatisfy` isValidClues
    context "updating the Clue for an Axis of some Clues with the Clue for that Axis" $ do
      it "returns the original Clues" $ property $ do
        \b p -> updateClueAt b p (clueAt b p) `shouldBe` b
    context "updating a Clue for an Axis of some Clues twice" $ do
      it "returns the same Clues that result from only updating the Clues the second time" $ property $ do
        \b p t t' -> updateClueAt (updateClueAt b p t) p t' `shouldBe` updateClueAt b p t'
