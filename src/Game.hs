module Game
  ( Game (getGameBoard, getGamePartialBoard)
  , isValidGame
  , game
  , unGame
  , newGame
  , flipTileAt
  , isComplete
  ) where

import Board (Board)
import PartialBoard (PartialBoard, emptyBoard, flipTileAtWith, isCompleteWith, isConsistentWith)
import Position (Position)

-- A Game consisting of a Board and a consistent PartialBoard.
data Game = Game {
  getGameBoard        :: Board,
  getGamePartialBoard :: PartialBoard
} deriving (Eq, Ord, Show)

-- Checks whether the given Board and PartialBoard are valid as a Game.
isValidGame :: Game -> Bool
isValidGame = uncurry (flip isConsistentWith) . unGame

-- Constructor for a Game.
game :: Board -> PartialBoard -> Game
game b pb | isValidGame g = g
          | otherwise     = error "PartialBoard is not consistent with Board"
              where g = Game b pb

-- Deconstructor for a Game.
unGame :: Game -> (Board, PartialBoard)
unGame (Game b pb) = (b, pb)

-- Constructor for a new Game, with an empty PartialBoard.
newGame :: Board -> Game
newGame b = Game b emptyBoard

-- Flips a Tile of the PartialBoard at the given Position
flipTileAt :: Position -> Game -> Game
flipTileAt p g = Game b pb'
  where (b, pb) = unGame g
        pb'     = flipTileAtWith b p pb

-- Tests whether the PartialBoard has flipped all of the non-trivial Tiles in
-- the Board.
isComplete :: Game -> Bool
isComplete = uncurry (flip isCompleteWith) . unGame
