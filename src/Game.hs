module Game
  ( Game ()
  , game
  , unGame
  , newGame
  , flipTileAt
  , isComplete
  ) where

import Board (Board)
import PartialBoard (PartialBoard, emptyBoard, isConsistent)
import qualified PartialBoard as PB (flipTileAt, isComplete)
import Position (Position)
import Tile (Tile)

-- A Game consisting of a Board and a consistent PartialBoard.
data Game = Game Board PartialBoard
  deriving (Eq, Ord, Show)

-- Constructor for a Game.
game :: Board -> PartialBoard -> Game
game b pb | isConsistent pb b = Game b pb
          | otherwise         = error "PartialBoard is not consistent with Board"

-- Deconstructor for a Game.
unGame :: Game -> (Board, PartialBoard)
unGame (Game b pb) = (b, pb)

-- Constructor for a new Game, with an empty PartialBoard.
newGame :: Board -> Game
newGame b = Game b emptyBoard

-- Flips a Tile of the PartialBoard at the given Position
flipTileAt :: Game -> Position -> (Tile, Game)
flipTileAt g p = (t, Game b pb')
  where (b, pb)  = unGame g
        (t, pb') = PB.flipTileAt pb b p

-- Tests whether the PartialBoard has flipped all of the non-trivial Tiles in
-- the Board.
isComplete :: Game -> Bool
isComplete = uncurry (flip PB.isComplete) . unGame
