module GameSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances (genCompleteGame, genConsistentPartialBoard, genIncompleteGame, genInconsistentPartialBoard)
import Game
import PartialBoard (emptyBoard, isConsistent)
import qualified PartialBoard as PB (flipTileAt)

spec :: Spec
spec = do
  describe "game" $ do
    context "given a Board and a consistent PartialBoard" $ do
      it "is inverted by unGame" $ property $ do
        (b, pb) <- genConsistentPartialBoard
        return $ unGame (game b pb) `shouldBe` (b, pb)
    context "given a Board and an inconsistent PartialBoard" $ do
      it "returns an error" $ property $ do
        (b, pb) <- genInconsistentPartialBoard
        return $ evaluate (game b pb) `shouldThrow` errorCall "PartialBoard is not consistent with Board"

  describe "newGame" $ do
    it "is inverted by unGame" $ property $ do
      \b -> unGame (newGame b) `shouldBe` (b, emptyBoard)
    it "returns a Game with a consistent PartialBoard" $ property $ do
      \b -> newGame b `shouldSatisfy` (uncurry (flip isConsistent) . unGame)

  describe "flipTileAt" $ do
    it "does not modify the Board" $ property $ do
      \g p -> let (b, _) = unGame g in (fst $ unGame $ snd $ flipTileAt g p) `shouldBe` b
    it "flips the Tile at the given Position of the PartialBoard" $ property $ do
      \g p -> let (b, pb) = unGame g in (snd $ unGame $ snd $ flipTileAt g p) `shouldBe` (snd $ PB.flipTileAt pb b p)
    it "returns the Tile that was flipped" $ property $ do
      \g p -> let (b, pb) = unGame g in (fst $ flipTileAt g p) `shouldBe` (fst $ PB.flipTileAt pb b p)
    it "returns a Game with a consistent PartialBoard" $ property $ do
      \g p -> flipTileAt g p `shouldSatisfy` (uncurry (flip isConsistent) . unGame . snd)

  describe "isComplete" $ do
    context "given a complete PartialBoard" $ do
      it "returns True" $ property $ do
        g <- genCompleteGame
        return $ isComplete g `shouldBe` True
    context "given an incomplete PartialBoard" $ do
      it "returns False" $ property $ do
        g <- genIncompleteGame
        return $ isComplete g `shouldBe` False
