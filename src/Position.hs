module Position
  ( Position
  , position
  , unPosition
  , columnOf
  , rowOf
  , row
  , rows
  , positionsByRow
  , column
  , columns
  , positionsByColumn
  ) where

import Data.Array (Ix)

import Coordinate

-- A Position on a 5x5 Board
data Position = P Coordinate Coordinate
  deriving (Ix, Eq, Ord, Show)

instance Bounded Position where
  minBound = position minBound minBound
  maxBound = position maxBound maxBound

instance Enum Position where
  toEnum x   | x >= 0 && x < 25 = position (coordinate $ x `mod` 5) (coordinate $ x `div` 5)
             | otherwise        = error "Position out of bounds"
  fromEnum p = 5 * (unCoordinate $ rowOf p) + (unCoordinate $ columnOf p)

-- Constructor for a Position.
position :: Coordinate -> Coordinate -> Position
position = P

-- Deconstructor for a Position.
unPosition :: Position -> (Coordinate, Coordinate)
unPosition (P x y) = (x, y)

-- Returns the x-coordinate or column of a Position.
columnOf :: Position -> Coordinate
columnOf (P x _) = x

-- Returns the y-coordinate or row of a Position.
rowOf :: Position -> Coordinate
rowOf (P _ y) = y

-- Returns a list of all Positions with the given row Coordinate, in ascending
-- order of column Coordinate.
row :: Coordinate -> [Position]
row y = [position x y | x <- coordinates]

-- Returns a list of all rows, in ascending order of row Coordinate.
rows :: [[Position]]
rows = [row y | y <- coordinates]

-- Returns a list of all Positions, in ascending order of row and then column
-- Coordinate.
positionsByRow :: [Position]
positionsByRow = concat rows

-- Returns a list of all columns, in ascending order of column Coordinate.
column :: Coordinate -> [Position]
column x = [position x y | y <- coordinates]

-- Returns a list of all columns, in ascending order of column Coordinate.
columns :: [[Position]]
columns = [column x | x <- coordinates]

-- Returns a list of all Positions, in ascending order of column and then row
-- Coordinate.
positionsByColumn :: [Position]
positionsByColumn = concat columns
