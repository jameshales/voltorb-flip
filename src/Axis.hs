module Axis
  ( Axis(..)
  , axes
  ) where

import Data.Ix (Ix, index, inRange, range, rangeSize)

import Coordinate (Coordinate, coordinate, unCoordinate)

data Axis = Row Coordinate | Column Coordinate
  deriving (Eq, Ord, Show)

instance Bounded Axis where
  minBound = Row $ coordinate 0
  maxBound = Column $ coordinate 4

instance Enum Axis where
  toEnum x | x < 0      = error "Axis out of bounds"
           | x < 5      = Row $ coordinate x
           | x < 10     = Column $ coordinate $ x - 5
           | otherwise  = error "Axis out of bounds"
  fromEnum (Row c)      = unCoordinate c
  fromEnum (Column c)   = unCoordinate c + 5

instance Ix Axis where
  range (l, u)              = [l..u]
  index (l, u) a@(Row c)    | inRange (l, u) a = (fromEnum c) - (fromEnum l)
                            | otherwise = error "Axis out of range"
  index (l, u) a@(Column c) | inRange (l, u) a = 5 + (fromEnum c) - (fromEnum l)
                            | otherwise = error "Axis out of range"
  inRange (l, u) i          = l <= i && i <= u
  rangeSize (l, u)          | l <= u    = fromEnum u - fromEnum l + 1
                            | otherwise = 0

axes :: [Axis]
axes = [minBound .. maxBound]
