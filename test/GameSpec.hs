module GameSpec (spec) where

import Control.Exception (evaluate)
import Test.Hspec
import Test.QuickCheck

import Game
import PartialBoard (emptyBoard, flipTileAtWith)

import PartialBoardSpec (completePartialBoard, consistentPartialBoard, incompletePartialBoard, inconsistentPartialBoard)

completeGame :: Gen Game
completeGame = uncurry game <$> completePartialBoard

incompleteGame :: Gen Game
incompleteGame = uncurry game <$> incompletePartialBoard

instance Arbitrary Game where
  arbitrary = uncurry game <$> consistentPartialBoard

spec :: Spec
spec = do
  describe "game" $ do
    context "given a Board and a consistent PartialBoard" $ do
      it "is inverted by unGame" $ property $ do
        (b, pb) <- consistentPartialBoard
        return $ unGame (game b pb) `shouldBe` (b, pb)
    context "given a Board and an inconsistent PartialBoard" $ do
      it "returns an error" $ property $ do
        (b, pb) <- inconsistentPartialBoard
        return $ evaluate (game b pb) `shouldThrow` errorCall "PartialBoard is not consistent with Board"

  describe "newGame" $ do
    it "returns a valid Game" $ property $ do
      \b -> newGame b `shouldSatisfy` isValidGame
    it "is inverted by unGame" $ property $ do
      \b -> unGame (newGame b) `shouldBe` (b, emptyBoard)

  describe "flipTileAt" $ do
    it "returns a valid Game" $ property $ do
      \g p -> flipTileAt g p `shouldSatisfy` isValidGame
    it "does not modify the Board" $ property $ do
      \g p -> let (b, _) = unGame g in (getGameBoard $ flipTileAt p g) `shouldBe` b
    it "flips the Tile at the given Position of the PartialBoard" $ property $ do
      \g p -> let (b, pb) = unGame g in (getGamePartialBoard $ flipTileAt p g) `shouldBe` flipTileAtWith b p pb

  describe "isComplete" $ do
    context "given a complete PartialBoard" $ do
      it "returns True" $ property $ do
        g <- completeGame
        return $ isComplete g `shouldBe` True
    context "given an incomplete PartialBoard" $ do
      it "returns False" $ property $ do
        g <- incompleteGame
        return $ isComplete g `shouldBe` False
