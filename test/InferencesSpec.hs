module InferencesSpec (spec) where

import Control.Exception (evaluate)
import qualified Data.Set as Set (fromList, intersection, member, singleton, size)
import Test.Hspec
import Test.QuickCheck

import Axis (Axis(..))
import Board (Board, tileAt)
import Coordinate (unCoordinate)
import Game (Game(..))
import Inferences
import PartialBoard (maybeTileAt)
import PartialGame (PartialGame(..), partialGameFor)
import Position (getX, getY, positionsByColumn)
import Tile (tile)

import ArrayGenerators (completeBoundedArray, incompleteBoundedArray)
import GameSpec ()
import PartialGameSpec (completePartialGame)
import PositionSpec ()

instance Arbitrary Inferences where
  arbitrary = inferences <$> completeBoundedArray

consistentWith :: PartialGame -> Inferences -> Bool
consistentWith pg is = all (\p -> maybe True (\t -> Set.singleton t == tileSetAt is p) $ maybeTileAt pb p) positionsByColumn
  where pb = getPartialGameBoard pg

consistentWithBoard :: Board -> Inferences -> Bool
consistentWithBoard b is = all (\p -> Set.member (tileAt b p) (tileSetAt is p)) positionsByColumn

isComplete :: Inferences -> Bool
isComplete is = all (\p -> let tss = tileSetAt is p in tss == (Set.fromList [tile 0, tile 1]) || Set.size tss == 1) positionsByColumn

spec :: Spec
spec = do
  describe "inferences" $ do
    context "given an Array with complete bounds" $ do
      it "is inverted by unInferences" $ property $
        forAll completeBoundedArray $ \arr ->
          unInferences (inferences arr) `shouldBe` arr
    context "given an Array with incomplete bounds" $ do
      it "returns an error" $ property $
        forAll incompleteBoundedArray $ \arr ->
          evaluate (inferences arr) `shouldThrow` errorCall "Array does not have complete bounds"

  describe "inferencesByColumn" $ do
    it "returns Inferences based on the inferencesForAxis for each Column in the given PartialGame" $ property $
      \pg p ->
      let a = Column $ getX p in
        tileSetAt (inferencesByColumn pg) p `shouldBe` (inferencesForAxis pg a) !! (unCoordinate $ getY p)
    it "returns Inferences that are consistent with the given PartialGame" $ property $
      \pg -> inferencesByColumn pg `shouldSatisfy` consistentWith pg
    it "returns Inferences that are consistent with the given Game" $ property $
      \g -> inferencesByColumn (partialGameFor g) `shouldSatisfy` consistentWithBoard (getGameBoard g)

  describe "inferencesByRow" $ do
    it "returns Inferences based on the inferencesForAxis for each Row in the given PartialGame" $ property $
      \pg p ->
      let a = Row $ getY p in
        tileSetAt (inferencesByRow pg) p `shouldBe` (inferencesForAxis pg a) !! (unCoordinate $ getX p)
    it "returns Inferences that are consistent with the given PartialGame" $ property $
      \pg -> inferencesByRow pg `shouldSatisfy` consistentWith pg
    it "returns Inferences that are consistent with the given Game" $ property $
      \g -> inferencesByRow (partialGameFor g) `shouldSatisfy` consistentWithBoard (getGameBoard g)

  describe "intersectInferences" $ do
    it "returns the point-wise intersection of the Tile sets in a pair of Inferences" $ property $
      \is is' p ->
        tileSetAt (is `intersectInferences` is') p `shouldBe` (tileSetAt is p `Set.intersection` tileSetAt is' p)

  describe "inferencesFor" $ do
    it "returns Inferences that are consistent with the given PartialGame" $ property $
      \pg -> inferencesFor pg `shouldSatisfy` consistentWith pg
    it "returns Inferences that are consistent with the given Game" $ property $
      \g -> inferencesFor (partialGameFor g) `shouldSatisfy` consistentWithBoard (getGameBoard g)
    context "given a completed PartialGame" $ do
      it "returns Inferences with the only ambiguity being between Voltorb and 1-Tiles" $ property $
        forAll completePartialGame $ \pg ->
          inferencesFor pg `shouldSatisfy` isComplete
