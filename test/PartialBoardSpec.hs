module PartialBoardSpec (spec) where

import Data.Array ((!))
import Test.Hspec
import Test.QuickCheck

import ArbitraryInstances (genMaybeTileArray)
import PartialBoard

spec :: Spec
spec = do
  describe "partialBoard" $
    it "is inverted by unPartialBoard" $ property $ do
      a <- genMaybeTileArray
      return $ unPartialBoard (partialBoard a) `shouldBe` a

  describe "emptyBoard" $
    it "contains Nothing" $ property $ do
      \p -> (unPartialBoard emptyBoard) ! p `shouldBe` Nothing
