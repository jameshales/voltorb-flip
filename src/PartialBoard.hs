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
  , flipTileAtWith
  , flipTilesAtWith
  , isConsistentWith
  , isCompleteWith
  , isConsistentWithClues
  , isCompleteWithClues
  ) where

import Data.Array (Array, array, bounds, (!), (//))
import Data.Char (intToDigit)

import Axis (Axis, axes)
import Board (Board, findRequiredTiles, tileAt, tilesAt)
import Clue (Clue(getSumOfTiles, getNumberOfVoltorbs))
import Clues (Clues, clueAt)
import Tile (Tile, unTile, numberOfFlippedTiles, numberOfFlippedVoltorbs, sumOfFlippedTiles)
import Position (Position, axis, positionsByColumn, rows)

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
flipTileAtWith :: Board -> Position -> PartialBoard -> PartialBoard
flipTileAtWith b p pb = flipTilesAtWith pb b [p]

-- Flips the Tiles at the given Positions of a Board in the given PartialBoard.
flipTilesAtWith :: PartialBoard -> Board -> [Position] -> PartialBoard
flipTilesAtWith pb b ps = updateMaybeTilesAt pb $ zip ps $ map Just $ tilesAt b ps

-- Tests whether the flipped Tiles in the PartialBoard are consistent with the
-- given Board.
isConsistentWith :: PartialBoard -> Board -> Bool
isConsistentWith pb b = all (\p -> maybe True (== tileAt b p) $ maybeTileAt pb p) $ positionsByColumn

-- Tests whether the PartialBoard has flipped all of the non-trivial Tiles in
-- the given Board.
isCompleteWith :: PartialBoard -> Board -> Bool
isCompleteWith pb b = all (\p -> maybeTileAt pb p == Just (tileAt b p)) $  findRequiredTiles b

-- Tests whether the list of Tiles is consistent with the given Clue. This is a
-- heuristic that may have false positives, but it will not have false
-- negatives.
isConsistentWithClue :: [Maybe Tile] -> Clue -> Bool
isConsistentWithClue ts c = s' <= s && n' <= n
  where s      = getSumOfTiles c
        n      = getNumberOfVoltorbs c
        s'     = sumOfFlippedTiles ts
        n'     = numberOfFlippedVoltorbs ts

-- Tests whether the list of Tiles is complete with the given Clue.
isCompleteWithClue :: [Maybe Tile] -> Clue -> Bool
isCompleteWithClue ts c = sumOfUnflippedTiles == numberOfFlippableTiles
  where sumOfUnflippedTiles    = getSumOfTiles c - sumOfFlippedTiles ts
        numberOfUnflippedTiles = 5 - numberOfFlippedTiles ts
        numberOfFlippableTiles = numberOfUnflippedTiles - getNumberOfVoltorbs c

-- Tests whether the PartialBoard is consistent with the given Clues at the
-- given Axis. This is a heuristic that may have false positives, but it will
-- not have false negatives.
isConsistentWithCluesAtAxis :: PartialBoard -> Clues -> Axis -> Bool
isConsistentWithCluesAtAxis pb cs a = (maybeTilesAt pb $ axis a) `isConsistentWithClue` clueAt cs a

-- Tests whether the PartialBoard is complete with the given Clues at the given
-- Axis.
isCompleteWithCluesAtAxis :: PartialBoard -> Clues -> Axis -> Bool
isCompleteWithCluesAtAxis pb cs a = (maybeTilesAt pb $ axis a) `isCompleteWithClue` clueAt cs a

-- Tests whether the PartialBoard is consistent with the given Clues. This is a
-- heuristic that may have false positives, but no false negatives.
isConsistentWithClues :: PartialBoard -> Clues -> Bool
isConsistentWithClues pb cs = all (isConsistentWithCluesAtAxis pb cs) axes

-- Tests whether the PartialBoard is complete with the given Clues.
isCompleteWithClues :: PartialBoard -> Clues -> Bool
isCompleteWithClues pb cs = all (isCompleteWithCluesAtAxis pb cs) axes
