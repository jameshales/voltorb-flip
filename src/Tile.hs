module Tile
  ( Tile ()
  , tile
  , unTile
  , voltorb
  , isVoltorb
  , isOptional
  , isRequired
  , tiles
  , numberOfVoltorbs
  , sumOfTiles
  , numberOfFlippedTiles
  , numberOfFlippedVoltorbs
  , sumOfFlippedTiles
  , clueFor
  ) where

import Data.Maybe (catMaybes)

import Clue (Clue, clue)

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

-- Returns True iff the given Tile is a 0-Tile.
isVoltorb :: Tile -> Bool
isVoltorb = (== voltorb)

-- Returns true iff the given Tile is a 1-Tile.
isOptional :: Tile -> Bool
isOptional = (== tile 1)

-- Returns true iff the given Tile is a 2- or 3-Tile
isRequired :: Tile -> Bool
isRequired = (>= 2) . unTile

-- A list of all Tiles in ascending order.
tiles :: [Tile]
tiles = [minBound..maxBound]

-- Counts the Voltorb Tiles in the given list.
numberOfVoltorbs :: [Tile] -> Int
numberOfVoltorbs = length . filter (== voltorb)

-- Sums the Tile values in the given list.
sumOfTiles :: [Tile] -> Int
sumOfTiles = sum . map unTile

-- Counts the flipped Tiles in the given list.
numberOfFlippedTiles :: [Maybe Tile] -> Int
numberOfFlippedTiles = length . catMaybes

-- Counts the flipped Voltorb Tiles in the given list.
numberOfFlippedVoltorbs :: [Maybe Tile] -> Int
numberOfFlippedVoltorbs = numberOfVoltorbs . catMaybes

-- Sums the values of the flipped Tiles in the given list.
sumOfFlippedTiles :: [Maybe Tile] -> Int
sumOfFlippedTiles = sumOfTiles . catMaybes

-- Calculates the Clue for a list of Tiles.
clueFor :: [Tile] -> Clue
clueFor ts = clue (sumOfTiles ts) (numberOfVoltorbs ts)
