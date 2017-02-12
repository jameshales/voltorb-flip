module PartialBoard
  ( PartialBoard()
  , partialBoard
  , unPartialBoard
  , emptyBoard
  , maybeTileAt
  , maybeTilesAt
  , flipTileAt
  , flipTilesAt
  , isComplete
  ) where

import Data.Array (Array, array, bounds, (!), (//))

import Board (Board, findNonTrivialTiles, tileAt)
import Tile (Tile)
import Position (Position, positionsByColumn)

-- A partially flipped 5x5 Board of Tiles
data PartialBoard = PartialBoard (Array Position (Maybe Tile))
  deriving (Eq, Ord, Show)

-- Constructor for a PartialBoard
partialBoard :: Array Position (Maybe Tile) -> PartialBoard
partialBoard a | bounds a == (minBound, maxBound) = PartialBoard a
               | otherwise = error "Array does not have full bounds"

-- Deconstructor for a PartialBoard
unPartialBoard :: PartialBoard -> Array Position (Maybe Tile)
unPartialBoard (PartialBoard a) = a

-- An empty PartialBoard
emptyBoard :: PartialBoard
emptyBoard = partialBoard $ array (minBound, maxBound) $ positionsByColumn `zip` repeat Nothing

-- Returns the Maybe Tile at the given Position of a PartialBoard.
maybeTileAt :: PartialBoard -> Position -> Maybe Tile
maybeTileAt pb p = unPartialBoard pb ! p

-- Returns the Maybe Tiles at the given Positions of a PartialBoard.
maybeTilesAt :: PartialBoard -> [Position] -> [Maybe Tile]
maybeTilesAt pb ps = map (maybeTileAt pb) ps

-- Flips the Tile at the given Position of a Board in the given PartialBoard,
-- and returns the Tile that was flipped.
flipTileAt :: PartialBoard -> Board -> Position -> (Tile, PartialBoard)
flipTileAt pb b p = (t, partialBoard $ unPartialBoard pb // [(p, Just t)])
  where t = tileAt b p

-- Flips the Tiles at the given Positions of a Board in the given PartialBoard.
flipTilesAt :: PartialBoard -> Board -> [Position] -> ([Tile], PartialBoard)
flipTilesAt pb b ps = foldr flipTileAt' (([], pb)) ps
  where flipTileAt' p (ts, pb') = let (t, pb'') = flipTileAt pb' b p in (t:ts, pb'')

-- Tests whether the PartialBoard has flipped all of the non-trivial Tiles in
-- the given Board.
isComplete :: PartialBoard -> Board -> Bool
isComplete pb b = all (\p -> maybeTileAt pb p == Just (tileAt b p)) $  findNonTrivialTiles b
