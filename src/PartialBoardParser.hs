{-# LANGUAGE FlexibleContexts #-}

module PartialBoardParser (pPartialBoard) where

import Data.Array (array)
import Data.Char (digitToInt)
import Text.Parsec

import PartialBoard (PartialBoard, partialBoard)
import Position (Position, rows)
import Tile (Tile, tile)

pTile :: Stream s m Char => ParsecT s u m Tile
pTile = fmap (tile . digitToInt) $ oneOf "0123"

pMaybeTile :: Stream s m Char => ParsecT s u m (Maybe Tile)
pMaybeTile = fmap Just pTile <|> fmap (const Nothing) (char '-')

pRow :: Stream s m Char => [Position] -> ParsecT s u m [(Position, Maybe Tile)]
pRow ps = sequence $ map (flip fmap pMaybeTile . (,)) ps

pPartialBoard :: Stream s m Char => ParsecT s u m PartialBoard
pPartialBoard = fmap (partialBoard . array (minBound, maxBound) . concat) $ sequence $ flip map rows $ \ps -> do
  as <- pRow ps
  _  <- newline
  return as
