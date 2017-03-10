module GameSpec
  ( spec
  , completeGame
  , incompleteGame
  ) where

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
      it "is inverted by unGame" $ property $
        forAll (consistentPartialBoard) $ \(b, pb) ->
          unGame (game b pb) `shouldBe` (b, pb)
    context "given a Board and an inconsistent PartialBoard" $ do
      it "returns an error" $ property $
        forAll (inconsistentPartialBoard) $ \(b, pb) ->
          evaluate (game b pb) `shouldThrow` errorCall "PartialBoard is not consistent with Board"

  describe "newGame" $ do
    it "returns a valid Game" $ property $
      \b -> newGame b `shouldSatisfy` isValidGame
    it "is inverted by unGame" $ property $
      \b -> unGame (newGame b) `shouldBe` (b, emptyBoard)

  describe "flipTileAt" $ do
    it "returns a valid Game" $ property $
      \g p -> flipTileAt g p `shouldSatisfy` isValidGame
    it "does not modify the Board" $ property $
      \g p -> let (b, _) = unGame g in (getGameBoard $ flipTileAt p g) `shouldBe` b
    it "flips the Tile at the given Position of the PartialBoard" $ property $
      \g p -> let (b, pb) = unGame g in (getGamePartialBoard $ flipTileAt p g) `shouldBe` flipTileAtWith b p pb

  describe "isComplete" $ do
    context "given a complete PartialBoard" $ do
      it "returns True" $ property $
        forAll (completeGame) $ \g ->
          isComplete g `shouldBe` True
    context "given an incomplete PartialBoard" $ do
      it "returns False" $ property $
        forAll (incompleteGame) $ \g ->
          isComplete g `shouldBe` False
