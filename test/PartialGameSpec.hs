module PartialGameSpec
  ( spec
  , completePartialGame
  , incompletePartialGame
  ) where

import Test.Hspec
import Test.QuickCheck

import Board (cluesFor)
import Game (Game(..))
import PartialGame

import CluesSpec ()
import GameSpec (completeGame, incompleteGame)
import PartialBoardSpec ()

completePartialGame :: Gen PartialGame
completePartialGame = partialGameFor <$> completeGame

incompletePartialGame :: Gen PartialGame
incompletePartialGame = partialGameFor <$> incompleteGame

instance Arbitrary PartialGame where
  arbitrary = partialGameFor <$> arbitrary

spec :: Spec
spec = do
  describe "partialGame" $ do
    it "is inverted by unPartialGame" $ property $
      \pb cs -> unPartialGame (partialGame pb cs) `shouldBe` (pb, cs)

  describe "getPartialGameBoard" $ do
    it "gets the PartialBoard contained in a PartialGame" $ property $
      \pb cs -> getPartialGameBoard (partialGame pb cs) `shouldBe` pb

  describe "getPartialGameClues" $ do
    it "gets the Clues contained in a PartialGame" $ property $
      \pb cs -> getPartialGameClues (partialGame pb cs) `shouldBe` cs

  describe "partialGameFor" $ do
    it "returns a PartialGame containing the PartialBoard from the given Game" $ property $
      \g -> getPartialGameBoard (partialGameFor g) `shouldBe` getGamePartialBoard g
    it "returns a PartialGame containing the Clues from the given Game" $ property $
      \g -> getPartialGameClues (partialGameFor g) `shouldBe` cluesFor (getGameBoard g)
