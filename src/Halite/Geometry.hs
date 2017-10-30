module Halite.Geometry where

import Halite.Types
import Numeric.LinearAlgebra

data Line = Line Point Point
  deriving (Eq, Ord, Read, Show)

data Circle = Circle Point Double

-- | Does a 'Line' intersect a 'Circle' in Euclidean space?
-- https://stackoverflow.com/a/1079478
lineIntersectsCircle :: Line -> Circle -> Maybe (Point, Point)
lineIntersectsCircle (Line a b) (Circle c r) =
  case norm_2 (ad - c) <= r of
    False -> Nothing
    True  -> Just (cd + scale dx ab_hat, cd - scale dx ab_hat) -- move along ab vector by dx amount
  where
    ac = c - a
    ab = b - a
    ad = projectVector ac ab
    cd = ad - ac -- go from C to A to D
    dx = sqrt (r**2 - norm_2 cd) -- intersection point
    ab_hat = scale (recip $ norm_2 ab) ab

-- | Vector projection of first onto second.
-- see https://en.wikipedia.org/wiki/Vector_projection
projectVector :: Vector R -> Vector R -> Vector R
projectVector a b = scale a1 bhat
  where a1   = dot a bhat
        bhat = scale (recip $ norm_2 b) b
