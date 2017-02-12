module Board
  ( Board ()
  , board
  , unBoard
  , tileAt
  , tilesAt
  , findNonTrivialTiles
  , sumOfTilesAt
  , sumOfTilesAtRow
  , sumOfTilesAtColumn
  , numberOfVoltorbsAt
  , numberOfVoltorbsAtRow
  , numberOfVoltorbsAtColumn
  ) where

import Data.Array (Array, bounds, (!))

import Coordinate (Coordinate)
import Position (Position, column, positionsByColumn, row)
import Tile (Tile, isNonTrivial, numberOfVoltorbs, sumOfTiles)

-- A 5x5 Board of Tiles
data Board = Board (Array Position Tile)
  deriving (Eq, Ord, Show)

-- Constructor for a Board.
board :: Array Position Tile -> Board
board a | bounds a == (minBound, maxBound)  = Board a
        | otherwise                         = error "Array does not have full bounds"

-- Deconstructor for a Board.
unBoard :: Board -> Array Position Tile
unBoard (Board a) = a

-- Returns the Tile at the given Position of a Board.
tileAt :: Board -> Position -> Tile
tileAt b p = unBoard b ! p

-- Returns the Tiles in the given list of Positions.
tilesAt :: Board -> [Position] -> [Tile]
tilesAt b = map $ tileAt b

-- Finds the Positions that contain 2/3-Tiles.
findNonTrivialTiles :: Board -> [Position]
findNonTrivialTiles b = filter (isNonTrivial . tileAt b) positionsByColumn

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
