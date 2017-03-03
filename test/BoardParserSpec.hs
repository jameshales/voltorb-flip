module BoardParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Text.Parsec (parse)

import BoardParser

import BoardSpec ()

spec :: Spec
spec = do
  describe "pBoard" $ do
    it "inverts show" $ property $ do
      \b -> parse pBoard "" (show b) `shouldBe` Right b
