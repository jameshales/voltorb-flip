module Inferences
  ( Inferences
  , isValidInferences
  , inferences
  , unInferences
  , tileSetAt
  , inferencesForAxis
  , inferencesByColumn
  , inferencesByRow
  , intersectInferences
  , inferencesFor
  ) where

import Data.Array (Array, array, bounds, elems, listArray, (!))
import Data.Set (Set)
import qualified Data.Set as Set (intersection)

import Axis (Axis, columnAxes, rowAxes)
import Clues (clueAt)
import PartialBoard (maybeTilesAt)
import PartialGame (PartialGame, unPartialGame)
import Position (Position, axis)
import Tile (Tile, allConsistentFlippedTileSets)

data Inferences = Inferences (Array Position (Set Tile))
  deriving (Eq, Ord, Show)

-- Checks whether the given Inferences are valid
isValidInferences :: Inferences -> Bool
isValidInferences is = bounds (unInferences is) == (minBound, maxBound)

-- Constructor for Inferences
inferences :: Array Position (Set Tile) -> Inferences
inferences arr | isValidInferences is = is
               | otherwise            = error "Array does not have complete bounds"
                   where is = Inferences arr

-- Deconstructor for Inferences
unInferences :: Inferences -> Array Position (Set Tile)
unInferences (Inferences arr) = arr

-- Returns the set of Tiles at the given Position of some Inferences.
tileSetAt :: Inferences -> Position -> Set Tile
tileSetAt is p = unInferences is ! p

-- Returns the consistent flipped Tile sets for the given PartialGame,
-- considering the Tiles and the Clue for the given Axis.
inferencesForAxis :: PartialGame -> Axis -> [Set Tile]
inferencesForAxis pg a = allConsistentFlippedTileSets mts c
  where (pb, cs) = unPartialGame pg
        mts      = maybeTilesAt pb $ axis a
        c        = clueAt cs a

-- Returns the Inferences for a PartialGame, considering the Tiles and Clues
-- for the given list of Axes.
inferencesByAxes :: [Axis] -> PartialGame -> Inferences
inferencesByAxes as pg = inferences
  $ array (minBound, maxBound)
  $ concatMap (\a -> axis a `zip` inferencesForAxis pg a) as

-- Returns the Inferences for a PartialGame, considering the Tiles and Clues
-- for each Column.
inferencesByColumn :: PartialGame -> Inferences
inferencesByColumn = inferencesByAxes columnAxes

-- Returns the Inferences for a PartialGame, considering the Tiles and Clues
-- for each Row.
inferencesByRow :: PartialGame -> Inferences
inferencesByRow = inferencesByAxes rowAxes

-- Returns the Inferences formed by intersecting the Tile sets at corresponding
-- Positions of two sets of Inferences.
intersectInferences :: Inferences -> Inferences -> Inferences
intersectInferences is is' = inferences
  $ listArray (minBound, maxBound)
  $ map (uncurry Set.intersection)
  $ elems (unInferences is) `zip` elems (unInferences is')

-- Returns the Inferences for the given PartialGame.
inferencesFor :: PartialGame -> Inferences
inferencesFor pg = inferencesByColumn pg `intersectInferences` inferencesByRow pg
