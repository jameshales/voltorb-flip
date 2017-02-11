module Coordinate
  ( Coordinate ()
  , coordinate
  , unCoordinate
  , coordinates
  ) where

import Data.Array (Ix)

-- A Coordinate on a 5x5 Board
data Coordinate = Coordinate Int
  deriving (Ix, Eq, Ord, Show)

instance Bounded Coordinate where
  minBound = coordinate 0
  maxBound = coordinate 4

instance Enum Coordinate where
  toEnum   = coordinate
  fromEnum = unCoordinate

-- Constructor for a Coordinate.
-- Returns a Coordinate for integers in the range [0, 4].
-- Returns an error for integers outside of the range.
coordinate :: Int -> Coordinate
coordinate x | x >= 0 && x < 5 = Coordinate x
             | otherwise       = error "Coordinate out of bounds"

-- Deconstructor for a Tile.
unCoordinate :: Coordinate -> Int
unCoordinate (Coordinate x) = x

-- A list of all Coordinates in ascending order.
coordinates :: [Coordinate]
coordinates = [minBound..maxBound]
