module Tile
  ( Tile ()
  , tile
  , unTile
  , voltorb
  , isOptional
  , isRequired
  , tiles
  , sumOfTiles
  , numberOfVoltorbs
  ) where

-- A Tile on a Board can be a 0 (Voltorb), 1, 2, or 3
data Tile = Tile Int
  deriving (Eq, Ord, Show)

instance Bounded Tile where
  minBound = tile 0
  maxBound = tile 3

instance Enum Tile where
  toEnum   = tile
  fromEnum = unTile

-- Constructor for a Tile.
-- Returns a Tile for integers in the range [0, 3].
-- Returns an error for integers outside of the range.
tile :: Int -> Tile
tile x | x >= 0 && x <= 3 = Tile x
       | otherwise        = error "Tile out of bounds"

-- Deconstructor for a Tile.
unTile :: Tile -> Int
unTile (Tile x) = x

-- Voltorb (tile with value 0)
voltorb :: Tile
voltorb = tile 0

-- Returns true iff the given Tile is a 1-Tile.
isOptional :: Tile -> Bool
isOptional = (== 1) . unTile

-- Returns true iff the given Tile is a 2- or 3-Tile
isRequired :: Tile -> Bool
isRequired = (>= 2) . unTile

-- A list of all Tiles in ascending order.
tiles :: [Tile]
tiles = [minBound..maxBound]

-- Returns the sum of all Tile values in the given list.
sumOfTiles :: [Tile] -> Int
sumOfTiles = sum . map unTile

-- Returns the number of Voltorb Tiles in the given list.
numberOfVoltorbs :: [Tile] -> Int
numberOfVoltorbs = length . filter (== voltorb)
