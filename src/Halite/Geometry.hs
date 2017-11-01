module Halite.Geometry where

import Halite.Types
import Numeric.LinearAlgebra

data Line = Line Point Point
  deriving (Eq, Ord, Read, Show)

data Circle = Circle Point Double
  deriving (Eq, Ord, Read, Show)

-- | Distance between two points (2-norm)
--
-- python:
-- return math.sqrt((target.x - self.x) ** 2 + (target.y - self.y) ** 2)
distance :: Point -> Point -> Double
distance x y = norm_2 (y - x)

-- | Does a 'Line' intersect a 'Circle' in Euclidean space? Note we mean "falls within" the circle
-- https://stackoverflow.com/a/1079478
lineIntersectsCircle :: Line -> Circle -> Maybe (Point, Point)
lineIntersectsCircle (Line a b) (Circle c r) =
  -- if norm_2 cd <= r
  case norm_2 (ad - c) <= r of
    False -> Nothing
    True  -> Just (d + scale dx ab_hat, d - scale dx ab_hat) -- move along ab vector by dx amount
  where
    ac = c - a
    ab = b - a
    ad = projectVector ac ab
    cd = negate ac + ad -- go from C to A to D
    dx = sqrt (r**2 - norm_2 cd) -- intersection point
    d  = a + ad
    ab_hat = scale (recip $ norm_2 ab) ab

-- TODO Fix broken lineIntersectsCircle
{-segmentIntersectsCircle :: Line -> Circle -> Maybe (Point, Point)-}
{-segmentIntersectsCircle line circle@(Circle c r) =-}
  {-lineIntersectsCircle line circle >>= withinCircle where-}
    {-withinCircle :: (Point, Point) -> Maybe (Point, Point)-}
    {-withinCircle (a,b) = if distance c a <= r || distance c b <= r then Just (a, b) else Nothing-}

-- | Vector projection of first onto second.
-- see https://en.wikipedia.org/wiki/Vector_projection
projectVector :: Vector R -> Vector R -> Vector R
projectVector a b = scale a1 bhat
  where a1   = dot a bhat
        bhat = scale (recip $ norm_2 b) b
