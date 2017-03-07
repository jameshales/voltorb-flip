module Board
  ( Board ()
  , isValidBoard
  , board
  , unBoard
  , tileAt
  , updateTileAt
  , tilesAt
  , updateTilesAt
  , findVoltorbTiles
  , findOptionalTiles
  , findRequiredTiles
  , cluesFor
  ) where

import Data.Array (Array, array, bounds, (!), (//))
import Data.Char (intToDigit)

import Axis (axes)
import Clues (Clues, clues)
import Position (Position, axis, positionsByColumn, rows)
import Tile (Tile, clueFor, isVoltorb, isOptional, isRequired, unTile)

-- A 5x5 Board of Tiles
data Board = Board (Array Position Tile)
  deriving (Eq, Ord)

instance Show Board where
  show b =
    unlines $ map (map $ showTile . tileAt b) rows
      where showTile = intToDigit . unTile

-- Checks whether the given Array is valid as a Board.
isValidBoard :: Board -> Bool
isValidBoard b = bounds (unBoard b) == (minBound, maxBound)

-- Constructor for a Board.
board :: Array Position Tile -> Board
board a | isValidBoard b = b
        | otherwise      = error "Array does not have complete bounds"
            where b = Board a

-- Deconstructor for a Board.
unBoard :: Board -> Array Position Tile
unBoard (Board a) = a

-- Returns the Tile at the given Position of a Board.
tileAt :: Board -> Position -> Tile
tileAt b p = unBoard b ! p

-- Updates the Tile at the given Position of a Board.
updateTileAt :: Board -> Position -> Tile -> Board
updateTileAt b p t = updateTilesAt b [(p, t)]

-- Returns the Tiles in the given list of Positions.
tilesAt :: Board -> [Position] -> [Tile]
tilesAt b = map $ tileAt b

-- Updates the Tiles at the corresponding Positions of a Board.
updateTilesAt :: Board -> [(Position, Tile)] -> Board
updateTilesAt b as = Board $ (// as) $ unBoard b

-- Finds the Positions of a Board that satsify the given predicate.
findTiles :: (Tile -> Bool) -> Board -> [Position]
findTiles p b = filter (p . tileAt b) positionsByColumn

-- Finds the Positions that contain 0-Tiles.
findVoltorbTiles :: Board -> [Position]
findVoltorbTiles = findTiles isVoltorb

-- Finds the Positions that contain 1-Tiles.
findOptionalTiles :: Board -> [Position]
findOptionalTiles = findTiles isOptional

-- Finds the Positions that contain 2/3-Tiles.
findRequiredTiles :: Board -> [Position]
findRequiredTiles = findTiles isRequired

-- Returns the Clues for all Axes of a Board.
cluesFor :: Board -> Clues
cluesFor b = clues $ array (minBound, maxBound) $ map (\a -> (a, clueFor $ tilesAt b $ axis a)) axes
