{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryInstances (genTileArray, genMaybeTileArray) where

import Data.Array (Array, array)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, infiniteListOf)

import Board (Board, board)
import Coordinate (Coordinate, coordinates)
import Tile (Tile, tiles)
import PartialBoard (PartialBoard, partialBoard)
import Position (Position, positionsByColumn)

instance Arbitrary Tile where
  arbitrary = elements tiles

instance Arbitrary Coordinate where
  arbitrary = elements coordinates

instance Arbitrary Position where
  arbitrary = elements positionsByColumn

genTileArray :: Gen (Array Position Tile)
genTileArray = fmap (array (minBound, maxBound) . zip positionsByColumn) $ infiniteListOf arbitrary

instance Arbitrary Board where
  arbitrary = fmap board genTileArray

genMaybeTileArray :: Gen (Array Position (Maybe Tile))
genMaybeTileArray = fmap (array (minBound, maxBound) . zip positionsByColumn) $ infiniteListOf arbitrary

instance Arbitrary PartialBoard where
  arbitrary = fmap partialBoard genMaybeTileArray
