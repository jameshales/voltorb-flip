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
  , genColumn
  , genRow
  , genClueArray
  ) where

import Data.Array (Array, array)
import Test.QuickCheck
  ( Arbitrary
  , Gen
  , arbitrary
  , choose
  , elements
  , infiniteListOf
  , oneof
  , sublistOf
  , suchThat
  )

import Axis (Axis (Column, Row), axes)
import Board (Board, board, findOptionalTiles, findRequiredTiles, tileAt)
import Clue (Clue, clue)
import Clues (Clues, clues)
import Coordinate (Coordinate, coordinates)
import Game (Game, game)
import PartialBoard
  ( PartialBoard
  , emptyBoard
  , flipTilesAtWith
  , partialBoard
  , updateMaybeTileAt
  )
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
  pb <- fmap (flipTilesAtWith emptyBoard b) $ sublistOf positionsByColumn
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
  let pb = flipTilesAtWith emptyBoard b ps
  return (b, pb)

genIncompletePartialBoard :: Gen (Board, PartialBoard)
genIncompletePartialBoard = do
  b  <- arbitrary `suchThat` (not . null . findRequiredTiles)
  ps <- let ns = findRequiredTiles b in (++) <$> sublistOf (findOptionalTiles b) <*> sublistOf ns `suchThat` (/=) ns
  let pb = flipTilesAtWith emptyBoard b ps
  return (b, pb)

genCompleteGame :: Gen Game
genCompleteGame = uncurry game <$> genCompletePartialBoard

genIncompleteGame :: Gen Game
genIncompleteGame = uncurry game <$> genIncompletePartialBoard

instance Arbitrary Game where
  arbitrary = uncurry game <$> genConsistentPartialBoard

genColumn :: Gen Axis
genColumn = fmap Column arbitrary

genRow :: Gen Axis
genRow = fmap Row arbitrary

instance Arbitrary Axis where
  arbitrary = oneof [genColumn, genRow]

instance Arbitrary Clue where
  arbitrary = do
    s <- choose (0, 15)
    n <- choose (0, 5)
    return $ clue s n

genClueArray :: Gen (Array Axis Clue)
genClueArray = fmap (array (minBound, maxBound) . zip axes) $ infiniteListOf arbitrary

instance Arbitrary Clues where
  arbitrary = fmap clues genClueArray
