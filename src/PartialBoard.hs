module PartialBoard
  ( PartialBoard()
  , isValidPartialBoard
  , partialBoard
  , unPartialBoard
  , emptyBoard
  , maybeTileAt
  , updateMaybeTileAt
  , maybeTilesAt
  , updateMaybeTilesAt
  , flipTileAt
  , flipTilesAt
  , isConsistent
  , isComplete
  ) where

import Data.Array (Array, array, bounds, (!), (//))
import Data.Char (intToDigit)

import Board (Board, findRequiredTiles, tileAt)
import Tile (Tile, unTile)
import Position (Position, positionsByColumn, rows)

-- A partially flipped 5x5 Board of Tiles
data PartialBoard = PartialBoard (Array Position (Maybe Tile))
  deriving (Eq, Ord)

instance Show PartialBoard where
  show pb =
    unlines $ map (map $ showTile . maybeTileAt pb) rows
      where showTile = maybe '-' (intToDigit . unTile) 

-- Checks whether the given PartialBoard is valid.
isValidPartialBoard :: PartialBoard -> Bool
isValidPartialBoard pb = bounds (unPartialBoard pb) == (minBound, maxBound)

-- Constructor for a PartialBoard
partialBoard :: Array Position (Maybe Tile) -> PartialBoard
partialBoard a | isValidPartialBoard pb = pb
               | otherwise              = error "Array does not have full bounds"
                  where pb = PartialBoard a

-- Deconstructor for a PartialBoard
unPartialBoard :: PartialBoard -> Array Position (Maybe Tile)
unPartialBoard (PartialBoard a) = a

-- An empty PartialBoard
emptyBoard :: PartialBoard
emptyBoard = PartialBoard $ array (minBound, maxBound) $ positionsByColumn `zip` repeat Nothing

-- Returns the Maybe Tile at the given Position of a PartialBoard.
maybeTileAt :: PartialBoard -> Position -> Maybe Tile
maybeTileAt pb p = unPartialBoard pb ! p

-- Updates the Maybe Tile at the given Position of a PartialBoard.
updateMaybeTileAt :: PartialBoard -> Position -> Maybe Tile -> PartialBoard
updateMaybeTileAt pb p mt = updateMaybeTilesAt pb [(p, mt)]

-- Returns the Maybe Tiles at the given Positions of a PartialBoard.
maybeTilesAt :: PartialBoard -> [Position] -> [Maybe Tile]
maybeTilesAt pb ps = map (maybeTileAt pb) ps

-- Updates the Maybe Tiles at the given Positions of a PartialBoard.
updateMaybeTilesAt :: PartialBoard -> [(Position, Maybe Tile)] -> PartialBoard
updateMaybeTilesAt pb as = PartialBoard $ (// as) $ unPartialBoard pb

-- Flips the Tile at the given Position of a Board in the given PartialBoard,
-- and returns the Tile that was flipped.
flipTileAt :: PartialBoard -> Board -> Position -> (Tile, PartialBoard)
flipTileAt pb b p = (t, PartialBoard $ unPartialBoard pb // [(p, Just t)])
  where t = tileAt b p

-- Flips the Tiles at the given Positions of a Board in the given PartialBoard.
flipTilesAt :: PartialBoard -> Board -> [Position] -> ([Tile], PartialBoard)
flipTilesAt pb b ps = foldr flipTileAt' (([], pb)) ps
  where flipTileAt' p (ts, pb') = let (t, pb'') = flipTileAt pb' b p in (t:ts, pb'')

-- Tests whether the flipped Tiles in the PartialBoard are consistent with the
-- given Board.
isConsistent :: PartialBoard -> Board -> Bool
isConsistent pb b = all (\p -> maybe True (== tileAt b p) $ maybeTileAt pb p) $ positionsByColumn

-- Tests whether the PartialBoard has flipped all of the non-trivial Tiles in
-- the given Board.
isComplete :: PartialBoard -> Board -> Bool
isComplete pb b = all (\p -> maybeTileAt pb p == Just (tileAt b p)) $  findRequiredTiles b
