module Clue
  ( Clue()
  , clue
  , unClue
  , getSumOfTiles
  , getNumberOfVoltorbs
  ) where

data Clue = Clue {
  getSumOfTiles       :: Int,
  getNumberOfVoltorbs :: Int
} deriving (Eq, Ord, Show)

-- Constructor for a Clue.
clue :: Int -> Int -> Clue
clue s n | s < 0      = error "Sum of Tiles is less than 0"
         | n < 0      = error "Number of Voltorbs is less than 0"
         | s > 15     = error "Sum of Tiles is greater than 15"
         | n > 5      = error "Number of Voltorbs is greater than 5"
         | otherwise  = Clue s n

-- Deconstructor for a Clue.
unClue :: Clue -> (Int, Int)
unClue (Clue s n) = (s, n)
