module PartialBoard
  ( PartialBoard()
  , partialBoard
  , unPartialBoard
  , emptyBoard
  ) where

import Data.Array (Array, array, bounds)
import Tile
import Position

-- A partially flipped 5x5 Board of Tiles
data PartialBoard = PartialBoard (Array Position (Maybe Tile))

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
