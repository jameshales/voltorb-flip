module Board
  ( Board
  , board
  , unBoard
  , tileAt
  , tilesAt
  , sumOfTilesAt
  , sumOfTilesAtRow
  , sumOfTilesAtColumn
  , numberOfVoltorbsAt
  , numberOfVoltorbsAtRow
  , numberOfVoltorbsAtColumn
  ) where

import Data.Array (Array, array, (!))

import Coordinate
import Position
import Tile

-- A 5x5 Board of Tiles
data Board = B (Array Position Tile)

-- Constructor for a Board.
board :: [(Position, Tile)] -> Board
board as = B $ array (minBound, maxBound) as

-- Deconstructor for a Board.
unBoard :: Board -> Array Position Tile
unBoard (B a) = a

-- Returns the Tile at the given Position of a Board.
tileAt :: Board -> Position -> Tile
tileAt b p = unBoard b ! p

-- Returns the Tiles in the given list of Positions.
tilesAt :: Board -> [Position] -> [Tile]
tilesAt b = map $ tileAt b

-- Returns the sum of all Tile values in the given Positions of a Board.
sumOfTilesAt :: Board -> [Position] -> Int
sumOfTilesAt = (sumOfTiles .) . tilesAt

-- Returns the sum of all Tile values in the given row of a Board.
sumOfTilesAtRow :: Board -> Coordinate -> Int
sumOfTilesAtRow b = sumOfTilesAt b . row

-- Returns the sum of all Tile values in the given column of a Board.
sumOfTilesAtColumn :: Board -> Coordinate -> Int
sumOfTilesAtColumn b = sumOfTilesAt b . column

-- Returns the number of Voltorb Tiles in the given Positions of a Board.
numberOfVoltorbsAt :: Board -> [Position] -> Int
numberOfVoltorbsAt = (numberOfVoltorbs .) . tilesAt

-- Returns the number of Voltorb Tiles in the given row of a Board.
numberOfVoltorbsAtRow :: Board -> Coordinate -> Int
numberOfVoltorbsAtRow b = numberOfVoltorbsAt b . row

-- Returns the number of Voltorb Tiles in the given column of a Board.
numberOfVoltorbsAtColumn :: Board -> Coordinate -> Int
numberOfVoltorbsAtColumn b = numberOfVoltorbsAt b . column
