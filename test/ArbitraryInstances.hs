{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryInstances
  ( genTileArray
  , genMaybeTileArray
  , genConsistentPartialBoard
  , genInconsistentPartialBoard
  , genCompletePartialBoard
  , genIncompletePartialBoard
  , genCompleteGame
  , genIncompleteGame
  ) where

import Data.Array (Array, array)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, infiniteListOf, sublistOf, suchThat)

import Board (Board, board, findOptionalTiles, findRequiredTiles, tileAt)
import Coordinate (Coordinate, coordinates)
import Game (Game, game)
import PartialBoard (PartialBoard, emptyBoard, flipTilesAt, partialBoard, updateMaybeTileAt)
import Position (Position, positionsByColumn)
import Tile (Tile, tiles)

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

genConsistentPartialBoard :: Gen (Board, PartialBoard)
genConsistentPartialBoard = do
  b  <- arbitrary
  pb <- fmap (snd . flipTilesAt emptyBoard b) $ sublistOf positionsByColumn
  return (b, pb)

genInconsistentPartialBoard :: Gen (Board, PartialBoard)
genInconsistentPartialBoard = do
  b   <- arbitrary
  p   <- arbitrary
  t   <- arbitrary `suchThat` (tileAt b p /=)
  pb  <- fmap (\pb -> updateMaybeTileAt pb p $ Just t) arbitrary
  return (b, pb)

genCompletePartialBoard :: Gen (Board, PartialBoard)
genCompletePartialBoard = do
  b  <- arbitrary
  ps <- (findRequiredTiles b ++) <$> sublistOf (findOptionalTiles b)
  let pb = snd $ flipTilesAt emptyBoard b ps
  return (b, pb)

genIncompletePartialBoard :: Gen (Board, PartialBoard)
genIncompletePartialBoard = do
  b  <- arbitrary `suchThat` (not . null . findRequiredTiles)
  ps <- let ns = findRequiredTiles b in (++) <$> sublistOf (findOptionalTiles b) <*> sublistOf ns `suchThat` (/=) ns
  let pb = snd $ flipTilesAt emptyBoard b ps
  return (b, pb)

genCompleteGame :: Gen Game
genCompleteGame = uncurry game <$> genCompletePartialBoard

genIncompleteGame :: Gen Game
genIncompleteGame = uncurry game <$> genIncompletePartialBoard

instance Arbitrary Game where
  arbitrary = uncurry game <$> genConsistentPartialBoard
