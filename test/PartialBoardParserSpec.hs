module PartialBoardParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec (parse)

import PartialBoardParser

import PartialBoardSpec ()

spec :: Spec
spec = do
  describe "pPartialBoard" $ do
    it "inverts show" $ property $ do
      \pb -> parse pPartialBoard "" (show pb) `shouldBe` Right pb
