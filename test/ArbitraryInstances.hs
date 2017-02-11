{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryInstances () where

import Test.QuickCheck (Arbitrary, arbitrary, elements, infiniteListOf)

import Board (Board, board)
import Coordinate (Coordinate, coordinates)
import Tile (Tile, tiles)
import Position (Position, positionsByColumn)

instance Arbitrary Tile where
  arbitrary = elements tiles

instance Arbitrary Coordinate where
  arbitrary = elements coordinates

instance Arbitrary Position where
  arbitrary = elements positionsByColumn

instance Arbitrary Board where
  arbitrary = fmap (board . zip positionsByColumn) $ infiniteListOf arbitrary
