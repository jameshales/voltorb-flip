module Board
  ( Board ()
  , isValidBoard
  , board
  , unBoard
  , tileAt
  , updateTileAt
  , tilesAt
  , updateTilesAt
  , findOptionalTiles
  , findRequiredTiles
  , sumOfTilesAt
  , sumOfTilesAtRow
  , sumOfTilesAtColumn
  , sumOfTilesAtAxis
  , numberOfVoltorbsAt
  , numberOfVoltorbsAtRow
  , numberOfVoltorbsAtColumn
  , numberOfVoltorbsAtAxis
  ) where

import Data.Array (Array, bounds, (!), (//))
import Data.Char (intToDigit)

import Axis (Axis)
import Coordinate (Coordinate)
import Position (Position, axis, column, positionsByColumn, row, rows)
import Tile (Tile, isOptional, isRequired, numberOfVoltorbs, sumOfTiles, unTile)

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
        | otherwise      = error "Array does not have full bounds"
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

-- Finds the Positions that contain 1-Tiles.
findOptionalTiles :: Board -> [Position]
findOptionalTiles b = filter (isOptional . tileAt b) positionsByColumn

-- Finds the Positions that contain 2/3-Tiles.
findRequiredTiles :: Board -> [Position]
findRequiredTiles b = filter (isRequired . tileAt b) positionsByColumn

-- Returns the sum of all Tile values in the given Positions of a Board.
sumOfTilesAt :: Board -> [Position] -> Int
sumOfTilesAt = (sumOfTiles .) . tilesAt

-- Returns the sum of all Tile values in the given row of a Board.
sumOfTilesAtRow :: Board -> Coordinate -> Int
sumOfTilesAtRow b = sumOfTilesAt b . row

-- Returns the sum of all Tile values in the given column of a Board.
sumOfTilesAtColumn :: Board -> Coordinate -> Int
sumOfTilesAtColumn b = sumOfTilesAt b . column

-- Returns the sum of all Tile values in the given Axis of a Board.
sumOfTilesAtAxis :: Board -> Axis -> Int
sumOfTilesAtAxis b = sumOfTilesAt b . axis

-- Returns the number of Voltorb Tiles in the given Positions of a Board.
numberOfVoltorbsAt :: Board -> [Position] -> Int
numberOfVoltorbsAt = (numberOfVoltorbs .) . tilesAt

-- Returns the number of Voltorb Tiles in the given row of a Board.
numberOfVoltorbsAtRow :: Board -> Coordinate -> Int
numberOfVoltorbsAtRow b = numberOfVoltorbsAt b . row

-- Returns the number of Voltorb Tiles in the given column of a Board.
numberOfVoltorbsAtColumn :: Board -> Coordinate -> Int
numberOfVoltorbsAtColumn b = numberOfVoltorbsAt b . column

-- Returns the number of Voltorb Tiles in the given Axis of a Board.
numberOfVoltorbsAtAxis :: Board -> Axis -> Int
numberOfVoltorbsAtAxis b = numberOfVoltorbsAt b . axis
