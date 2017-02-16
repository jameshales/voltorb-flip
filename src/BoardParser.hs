{-# LANGUAGE FlexibleContexts #-}

module BoardParser (pBoard) where

import Data.Array (array)
import Data.Char (digitToInt)
import Text.Parsec

import Board (Board, board)
import Position (Position, rows)
import Tile (Tile, tile)

pTile :: Stream s m Char => ParsecT s u m Tile
pTile = fmap (tile . digitToInt) $ oneOf "0123"

pRow :: Stream s m Char => [Position] -> ParsecT s u m [(Position, Tile)]
pRow ps = sequence $ map (flip fmap pTile . (,)) ps

pBoard :: Stream s m Char => ParsecT s u m Board
pBoard = fmap (board . array (minBound, maxBound) . concat) $ sequence $ flip map rows $ \ps -> do
  as <- pRow ps
  _  <- newline
  return as
