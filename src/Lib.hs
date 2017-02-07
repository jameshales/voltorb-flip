module Lib
    ( Tile
    , tile
    , unTile
    , voltorb
    , tiles
    , Coordinate
    , coordinate
    , unCoordinate
    , coordinates
    , Position
    , position
    , unPosition
    , columnOf
    , rowOf
    , row
    , column
    , rows
    , columns
    , positionsByRow
    , positionsByColumn
    , Board
    , board
    , unBoard
    , sumOfTiles
    , sumOfTilesAt
    , sumOfTilesAtRow
    , sumOfTilesAtColumn
    , numberOfVoltorbs
    , numberOfVoltorbsAt
    , numberOfVoltorbsAtRow
    , numberOfVoltorbsAtColumn
    ) where

import Data.Array (Array, Ix, array, (!))

-- A Tile on a Board can be a 0 (Voltorb), 1, 2, or 3
data Tile = T Int
  deriving (Eq, Ord, Show)

instance Bounded Tile where
  minBound = tile 0
  maxBound = tile 3

instance Enum Tile where
  toEnum         = tile
  fromEnum (T x) = x

-- A Coordinate on a 5x5 Board
data Coordinate = C Int
  deriving (Ix, Eq, Ord, Show)

instance Bounded Coordinate where
  minBound = coordinate 0
  maxBound = coordinate 4

instance Enum Coordinate where
  toEnum          = coordinate
  fromEnum (C x)  = x

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

-- A 5x5 Board of Tiles
data Board = B (Array Position Tile)

-- Constructor for a Tile.
-- Returns a Tile for integers in the range [0, 3].
-- Returns an error for integers outside of the range.
tile :: Int -> Tile
tile x | x >= 0 && x <= 3 = T x
       | otherwise        = error "Tile out of bounds"

-- Deconstructor for a Tile.
unTile :: Tile -> Int
unTile (T x) = x


-- Voltorb (tile with value 0)
voltorb = tile 0

-- A list of all Tiles in ascending order.
tiles :: [Tile]
tiles = [minBound..maxBound]

-- Constructor for a Coordinate.
-- Returns a Coordinate for integers in the range [0, 4].
-- Returns an error for integers outside of the range.
coordinate :: Int -> Coordinate
coordinate x | x >= 0 && x < 5 = C x
             | otherwise       = error "Coordinate out of bounds"

-- Deconstructor for a Tile.
unCoordinate :: Coordinate -> Int
unCoordinate (C x) = x

-- A list of all Coordinates in ascending order.
coordinates :: [Coordinate]
coordinates = [minBound..maxBound]

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

-- Returns the sum of all Tile values in the given list.
sumOfTiles :: [Tile] -> Int
sumOfTiles = sum . map unTile

-- Returns the sum of all Tile values in the given Positions of a Board.
sumOfTilesAt :: Board -> [Position] -> Int
sumOfTilesAt = (sumOfTiles .) . tilesAt

-- Returns the sum of all Tile values in the given row of a Board.
sumOfTilesAtRow :: Board -> Coordinate -> Int
sumOfTilesAtRow b = sumOfTilesAt b . row

-- Returns the sum of all Tile values in the given column of a Board.
sumOfTilesAtColumn :: Board -> Coordinate -> Int
sumOfTilesAtColumn b = sumOfTilesAt b . column

-- Returns the number of Voltorb Tiles in the given list.
numberOfVoltorbs :: [Tile] -> Int
numberOfVoltorbs = length . filter (== voltorb)

-- Returns the number of Voltorb Tiles in the given Positions of a Board.
numberOfVoltorbsAt :: Board -> [Position] -> Int
numberOfVoltorbsAt = (numberOfVoltorbs .) . tilesAt

-- Returns the number of Voltorb Tiles in the given row of a Board.
numberOfVoltorbsAtRow :: Board -> Coordinate -> Int
numberOfVoltorbsAtRow b = numberOfVoltorbsAt b . row

-- Returns the number of Voltorb Tiles in the given column of a Board.
numberOfVoltorbsAtColumn :: Board -> Coordinate -> Int
numberOfVoltorbsAtColumn b = numberOfVoltorbsAt b . column
