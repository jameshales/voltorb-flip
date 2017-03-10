module PartialGame
  ( PartialGame(..)
  , partialGame
  , unPartialGame
  , partialGameFor
  ) where

import Board (cluesFor)
import Clues (Clues)
import Game (Game(..))
import PartialBoard (PartialBoard)

data PartialGame = PartialGame {
  getPartialGameBoard :: PartialBoard,
  getPartialGameClues :: Clues
} deriving (Eq, Ord, Show)

-- Constructor for a PartialGame
partialGame :: PartialBoard -> Clues -> PartialGame
partialGame pb cs = PartialGame pb cs

-- Deconstructor for a PartialGame
unPartialGame :: PartialGame -> (PartialBoard, Clues)
unPartialGame (PartialGame pb cs) = (pb, cs)

-- Returns the PartialGame formed by taking the PartialBoard of the given Game
-- and the Clues calculated from the Board of the given Game.
partialGameFor :: Game -> PartialGame
partialGameFor g = partialGame (getGamePartialBoard g) (cluesFor $ getGameBoard g)
