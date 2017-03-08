module TileSpec (spec) where

import Control.Exception (ErrorCall, evaluate)
import Control.Monad (mapM)
import Data.List (nub, sort)
import qualified Data.Set as Set (empty, member, singleton)
import Test.Hspec
import Test.QuickCheck

import Clue (Clue(..), clue)
import Tile

import ClueSpec ()

instance Arbitrary Tile where
  arbitrary = elements tiles

outOfBoundsError :: Selector ErrorCall
outOfBoundsError = errorCall "Tile out of bounds"

consistentFlippedTiles :: [Tile] -> Gen [Maybe Tile]
consistentFlippedTiles ts = mapM (\t -> elements [Nothing, Just t]) ts

inconsistentFlippedTiles :: [Tile] -> Gen [Maybe Tile]
inconsistentFlippedTiles ts = (vectorOf (length ts) arbitrary) `suchThat` (any (uncurry inconsistent) . zip ts)
  where inconsistent t mt = mt /= Nothing && mt /= Just t

flippedTiles :: [Tile] -> Gen [Maybe Tile]
flippedTiles ts = oneof [consistentFlippedTiles ts, inconsistentFlippedTiles ts]

strictPrefixOf :: [a] -> Gen [a]
strictPrefixOf xs = flip take xs <$> choose (0, length xs - 1)

spec :: Spec
spec = do
  describe "instance Enum Tile" $ do
    describe "succ" $ do
      it "returns an error on maxBound" $ do
        evaluate (succ (maxBound :: Tile)) `shouldThrow` outOfBoundsError
    describe "pred" $ do
      it "returns an error on minBound" $ do
        evaluate (pred (minBound :: Tile)) `shouldThrow` outOfBoundsError
    describe "toEnum" $ do
      context "when the value is less than 0" $ do
        it "returns an error" $ property $ do
          i <- choose (minBound, 0)
          return $ evaluate (toEnum i :: Tile) `shouldThrow` outOfBoundsError
      context "when the value is greater than 3" $ do
        it "returns an error" $ property $ do
          i <- choose (4, maxBound)
          return $ evaluate (toEnum i :: Tile) `shouldThrow` outOfBoundsError
      context "otherwise" $ do
        it "is inverted by fromEnum" $ property $ do
          i <- choose (0, 3)
          return $ (fromEnum $ (toEnum i :: Tile)) `shouldBe` i
    describe "fromEnum" $ do
      it "is inverted by toEnum" $ property $
        \t -> (toEnum $ fromEnum t :: Tile) `shouldBe` t

  describe "tile" $ do
    context "when the value is less than 0" $ do
      it "returns an error" $ property $ do
        i <- choose (minBound, 0)
        return $ evaluate (tile i) `shouldThrow` outOfBoundsError
    context "when the value is greater than 3" $ do
      it "returns an error" $ property $ do
        i <- choose (4, maxBound)
        return $ evaluate (tile i) `shouldThrow` outOfBoundsError
    context "otherwise" $ do
      it "is inverted by unTile" $ property $ do
        i <- choose (0, 3)
        return $ unTile (tile i) `shouldBe` i

  describe "isVoltorb" $ do
    context "given a 0-Tile" $ do
      it "returns True" $
        isVoltorb (tile 0) `shouldBe` True
    context "given a 1-Tile" $ do
      it "returns False" $
        isVoltorb (tile 1) `shouldBe` False
    context "given a 2-Tile" $ do
      it "returns False" $
        isVoltorb (tile 2) `shouldBe` False
    context "given a 3-Tile" $ do
      it "returns False" $
        isVoltorb (tile 3) `shouldBe` False

  describe "isOptional" $ do
    context "given a 0-Tile" $ do
      it "returns False" $
        isOptional (tile 0) `shouldBe` False
    context "given a 1-Tile" $ do
      it "returns True" $
        isOptional (tile 1) `shouldBe` True
    context "given a 2-Tile" $ do
      it "returns False" $
        isOptional (tile 2) `shouldBe` False
    context "given a 3-Tile" $ do
      it "returns False" $
        isOptional (tile 3) `shouldBe` False

  describe "isRequired" $ do
    context "given a 0-Tile" $ do
      it "returns False" $
        isRequired (tile 0) `shouldBe` False
    context "given a 1-Tile" $ do
      it "returns False" $
        isRequired (tile 1) `shouldBe` False
    context "given a 2-Tile" $ do
      it "returns True" $
        isRequired (tile 2) `shouldBe` True
    context "given a 3-Tile" $ do
      it "returns True" $
        isRequired (tile 3) `shouldBe` True

  describe "tiles" $ do
    it "returns a list of 4 tiles" $ do
      length tiles `shouldBe` 4
    it "returns a list of distinct tiles" $ do
      nub tiles `shouldBe` tiles
    it "returns a list of tiles in sorted order" $ do
      sort tiles `shouldBe` tiles

  describe "sumOfTiles" $ do
    context "given an empty list of Tiles" $
      it "returns 0" $
        sumOfTiles [] `shouldBe` 0
    context "given a non-empty list of Tiles" $
      it "adds the value of the Tile at the head of the list to the sumOfTiles of the tail of the list" $ property $
        \t ts -> sumOfTiles (t:ts) `shouldBe` (unTile t + sumOfTiles ts)

  describe "numberOfVoltorbs" $ do
    context "given an empty list of Tiles" $
      it "returns 0" $ do
        numberOfVoltorbs [] `shouldBe` 0
    context "given a non-empty list of Tiles" $ do
      context "if the head of the list is a Voltorb" $
        it "returns the numberOfVoltorbs in the tail of the list" $ property $
          \t ts -> t /= voltorb ==> numberOfVoltorbs (t:ts) `shouldBe` numberOfVoltorbs ts
      context "if the head of the list isn't a Voltorb" $
        it "adds 1 to the numberOfVoltorbs in the tail of the list" $ property $
          \ts -> numberOfVoltorbs (voltorb:ts) `shouldBe` numberOfVoltorbs ts + 1

  describe "clueFor" $ do
    context "given a list of 5 Tiles" $ do
      it "returns a Clue containing the sumOfTiles for the list of Tiles" $ property $
        forAll (vectorOf 5 arbitrary) $ \ts ->
          getSumOfTiles (clueFor ts) `shouldBe` sumOfTiles ts
      it "returns a Clue containing the numberOfVoltorbs for the list of Tiles" $ property $
        forAll (vectorOf 5 arbitrary) $ \ts ->
          getNumberOfVoltorbs (clueFor ts) `shouldBe` numberOfVoltorbs ts

  describe "isConsistentWithClue" $ do
    context "given a list of Tiles and the matching Clue" $ do
      it "returns True" $ property $
        forAll (vectorOf 5 arbitrary) $ \ts ->
        let c = clueFor ts in
          isConsistentWithClue c ts `shouldBe` True
    context "given a list of Tiles and a non-matching Clue" $ do
      it "returns False" $ property $
        forAll (vectorOf 5 arbitrary) $ \ts ->
        let c = clueFor ts in
        forAll (arbitrary `suchThat` (/= c)) $ \c' ->
          isConsistentWithClue c' ts `shouldBe` False

  describe "isConsistentWithFlippedTiles" $ do
    context "given a list of unflipped Tiles" $ do
      it "returns True" $ property $ \ts ->
        let mts = map (const Nothing) ts in
          isConsistentWithFlippedTiles mts ts `shouldBe` True
    context "given a list of Tiles with the same length as and consistent with the given list of flipped Tiles" $ do
      it "returns True" $ property $ \ts ->
        forAll (consistentFlippedTiles ts) $ \mts ->
          isConsistentWithFlippedTiles mts ts `shouldBe` True
    context "given a list of Tiles shorter than the given list of flipped Tiles" $ do
      it "returns False" $ property $
        forAll (listOf1 arbitrary) $ \ts ->
        forAll (flippedTiles ts) $ \mts ->
        forAll (strictPrefixOf ts) $ \ts' ->
          isConsistentWithFlippedTiles mts ts' `shouldBe` False
    context "given a list of Tiles longer than the given list of flipped Tiles" $ do
      it "returns False" $ property $
        forAll (listOf1 arbitrary) $ \ts ->
        forAll (flippedTiles ts) $ \mts ->
        forAll (strictPrefixOf mts) $ \mts' ->
          isConsistentWithFlippedTiles mts' ts `shouldBe` False
    context "given a list of Tiles inconsistent with the given flipped Tiles" $ do
      it "returns False" $ property $
        forAll (listOf1 arbitrary) $ \ts ->
        forAll (inconsistentFlippedTiles ts) $ \mts ->
          isConsistentWithFlippedTiles mts ts `shouldBe` False

  describe "allFlippedTiles" $ do
    context "given an empty list" $ do
      it "returns a list containing an empty list" $ do
        allFlippedTiles [] `shouldBe` [[]]
    context "given a list of flipped Tiles" $ do
      it "returns a list containing the given list of flipped Tiles" $ property $ \ts ->
        let mts = map Just ts in
          allFlippedTiles mts `shouldBe` [ts]
    context "given a list of partially flipped Tiles" $ do
      it "returns a list of 4^N lists of Tiles, where N is the number of unflipped Tiles" $ property $
        forAll (choose (0, 5)) $ \m ->
        forAll (vectorOf m arbitrary) $ \mts ->
        let n = length $ filter (== Nothing) mts in
          allFlippedTiles mts `shouldSatisfy` (== 4^n) . length
      it "returns a list of distinct lists of Tiles" $ property $
        forAll (choose (0, 5)) $ \m ->
        forAll (vectorOf m arbitrary) $ \mts ->
          nub (allFlippedTiles mts) `shouldBe` allFlippedTiles mts
      it "returns a list of lists of Tiles that are consistent with the given list of flipped Tiles" $ property $
        forAll (choose (0, 5)) $ \m ->
        forAll (vectorOf m arbitrary) $ \mts ->
          allFlippedTiles mts `shouldSatisfy` all (isConsistentWithFlippedTiles mts)

  describe "allConsistentFlippedTiles" $ do
    context "given an empty list and a zero Clue" $ do
      it "returns a list containing an empty list" $ do
        allConsistentFlippedTiles [] (clue 0 0) `shouldBe` [[]]
    context "given an empty list and a non-zero Clue" $ do
      it "returns an empty list" $ property $ \c ->
        c /= (clue 0 0) ==> allConsistentFlippedTiles [] c `shouldBe` []
    context "given a list of flipped Tiles and the corresponding Clue" $ do
      it "returns a list containing the given list of flipped Tiles" $ property $
        forAll (vectorOf 5 arbitrary) $ \ts ->
        let mts = map Just ts in
        let c   = clueFor ts in
          allConsistentFlippedTiles mts c `shouldBe` [ts]
    context "given a list of partially flipped Tiles and the Clue corresponding to a list of Tiles" $ do
      it "returns a list containing the original list of Tiles" $ property $
        forAll (vectorOf 5 arbitrary) $ \ts ->
        forAll (consistentFlippedTiles ts) $ \mts ->
        let c   = clueFor ts in
          allConsistentFlippedTiles mts c `shouldSatisfy` elem ts

  describe "allConsistentFlippedTileSets" $ do
    context "given an empty list" $ do
      it "returns an empty list" $ property $ \c ->
        allConsistentFlippedTileSets [] c `shouldBe` []
    context "given a list of partially flipped Tiles and a Clue" $ do
      it "returns a list with the same length as the original list of Tiles" $ property $
        forAll (vectorOf 5 arbitrary) $ \ts ->
        forAll (flippedTiles ts) $ \mts ->
        let c   = clueFor ts in
        let n   = length ts in
          allConsistentFlippedTileSets mts c `shouldSatisfy` ((== n) . length)
    context "given a list of flipped Tiles and the corresponding Clue" $ do
      it "returns a list of singleton Sets corresponding to the flipped Tiles" $ property $
        forAll (vectorOf 5 arbitrary) $ \ts ->
        let mts = map Just ts in
        let tss = map (Set.singleton) ts in
        let c   = clueFor ts in
          allConsistentFlippedTileSets mts c `shouldBe` tss
    context "given a list of flipped Tiles and a non-matching Clue" $ do
      it "returns a list of empty Sets" $ property $
        forAll (vectorOf 5 arbitrary) $ \ts ->
        let c   = clueFor ts in
        forAll (arbitrary `suchThat` (/= c)) $ \c' ->
        let mts = map Just ts in
        let tss = map (const Set.empty) ts in
          allConsistentFlippedTileSets mts c' `shouldBe` tss
    context "given a list of partially flipped Tiles and the corresponding Clue" $ do
      it "returns a list of Sets of Tiles that agree with the original list of Tiles" $ property $
        forAll (vectorOf 5 arbitrary) $ \ts ->
        forAll (consistentFlippedTiles ts) $ \mts ->
        let c   = clueFor ts in
          allConsistentFlippedTileSets mts c `shouldSatisfy` (all (uncurry Set.member) . zip ts)
