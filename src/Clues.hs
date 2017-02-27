module Clues
  ( Clues()
  , isValidClues
  , clues
  , unClues
  , clueAt
  , updateClueAt
  ) where

import Data.Array (Array, bounds, (!), (//))

import Axis (Axis)
import Clue (Clue)

data Clues = Clues (Array Axis Clue)
  deriving (Eq, Ord, Show)

-- Checks whether the given Clues are valid.
isValidClues :: Clues -> Bool
isValidClues c = bounds (unClues c) == (minBound, maxBound)

-- Constructor for Clues.
clues :: Array Axis Clue -> Clues
clues arr | isValidClues c  = c
          | otherwise       = error "Array does not have full bounds"
              where c = Clues arr

-- Deconstructor for Clues.
unClues :: Clues -> Array Axis Clue
unClues (Clues arr) = arr

-- Returns the Clue for the given Axis from the Clues.
clueAt :: Clues -> Axis -> Clue
clueAt c a = unClues c ! a

-- Updates the Clue at the given Axis of the Clues.
updateClueAt :: Clues -> Axis -> Clue -> Clues
updateClueAt cs a c = Clues (unClues cs // [(a, c)])
