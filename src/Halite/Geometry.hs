module Halite.Geometry where

import Halite.Types
import Numeric.LinearAlgebra
import Control.Applicative

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
intersectLineCircle :: Line -> Circle -> Maybe (Point, Point)
intersectLineCircle (Line a b) (Circle c r) =
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

-- Let line segment be defined by two points, a and b.
-- A point p lies on the line if both are true:
--  1. distance AP < length line
--  2. distance BP < length line
onSegment :: Point -> Line -> Bool
p `onSegment` (Line a b) =
  distance a p <= lineLength && distance b p <= lineLength
  where lineLength = distance a b
        lenAP = distance a p
        lenBP = distance b p

-- | Does a line *segment* intersect a circular area?
--
-- Yes, if:
--   1. Either endpoint is within the circle
--   2. Either intersection point of the *line* is on the line *segment*
intersectSegmentCircle :: Line -> Circle -> Maybe (Point, Point)
intersectSegmentCircle line@(Line a b) circle@(Circle c r) = do
  (x,y) <- intersectLineCircle line circle
  case withinCircle a || withinCircle b of
    True  -> return (x,y)
    False -> hasOneIntersection (x,y)

  where
    withinCircle :: Point -> Bool
    withinCircle x = distance c x <= r

    hasOneIntersection :: (Point, Point) -> Maybe (Point, Point)
    hasOneIntersection (a,b) =
      if (a `onSegment` line) || (b `onSegment` line) then Just (a, b) else Nothing

-- | Vector projection of first vector onto second.
-- see https://en.wikipedia.org/wiki/Vector_projection
projectVector :: Vector R -> Vector R -> Vector R
projectVector a b = scale a1 bhat
  where a1   = dot a bhat
        bhat = scale (recip $ norm_2 b) b
