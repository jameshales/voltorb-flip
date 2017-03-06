module ArrayGenerators
  ( completeBoundedArray
  , incompleteBoundedArray
  , distinctAssocs
  , distinctAssocsTuple
  ) where

import Data.Array (Array, listArray)
import Data.Function (on)
import Data.List (nubBy)
import Data.Ix (Ix)
import Test.QuickCheck

completeBoundedArray :: (Bounded i, Ix i, Arbitrary a) => Gen (Array i a)
completeBoundedArray = listArray (minBound, maxBound) <$> infiniteListOf arbitrary

incompleteBoundedArray :: (Bounded i, Ix i, Arbitrary i, Arbitrary a) => Gen (Array i a)
incompleteBoundedArray = listArray <$> arbitrary `suchThat` (/= (minBound, maxBound)) <*> infiniteListOf arbitrary

distinctAssocs :: (Arbitrary i, Eq i, Arbitrary a) => Gen [(i, a)]
distinctAssocs = nubBy ((==) `on` fst) <$> arbitrary

distinctAssocsTuple :: (Arbitrary i, Eq i, Arbitrary a) => Gen ([i], [a])
distinctAssocsTuple = unzip <$> distinctAssocs
