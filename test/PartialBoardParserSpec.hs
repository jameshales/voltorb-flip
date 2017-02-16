module PartialBoardParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec (parse)

import ArbitraryInstances ()
import PartialBoardParser

spec :: Spec
spec = do
  describe "pPartialBoard" $ do
    it "inverts show" $ property $ do
      \b -> parse pPartialBoard "" (show b) `shouldBe` Right b
