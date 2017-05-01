module VectorUtils
  (
    allEqual
  , argmax
  , flipMatrix
  ) where

import Prelude hiding (all, elem, head, flip, map, maximum, minimum, tail)
import Data.Matrix
import Data.Vector hiding (reverse, toList)

allEqual :: (Eq a) => Vector a -> Bool
allEqual xs = all (== head xs) (tail xs)

argmax :: (Ord b) => (a -> b) -> Vector a -> a
argmax f = Data.Vector.foldl1 (\acc x -> if f x > f acc then x else acc)

flipMatrix :: Matrix a -> Matrix a
flipMatrix m = transpose $ Data.Matrix.fromList r c (reverse $ toList m)
             where r = nrows m
                   c = ncols m
