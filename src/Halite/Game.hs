{-# LANGUAGE FlexibleContexts #-}
module Halite.Game where

import Linear (dot, V2(..), (^-^))

import Halite.Types
import Halite.Geometry
import Halite.Commands
import Control.Lens

-- | Intersection between a 'Line' and an 'Entity'
intersectLineEntity :: Entity t => Line -> t -> Maybe (Point, Point)
intersectLineEntity line = intersectSegmentCircle line . (\x -> Circle (x ^. pos) (x ^. radius + 0.1))

-- | List of obstacles on the straight line between two points.
obstaclesBetween :: GameMap -> Point -> Point -> [Either Planet Ship]
obstaclesBetween gameMap x y = allShips ++ allPlanets
  where
    f :: Entity t => t -> Bool
    f = maybe False (const True) . intersectLineEntity (Line x y)

    allShips   = map Right . filter f $ (gameMap ^. players . traverse . ships)
    allPlanets = map Left  . filter f $ (gameMap ^. planets)

vectorAngle :: Point -> Point -> Double
vectorAngle x y = -- note: zero vector angle is defined as pi/2 radians
  case sum x == 0 || sum y == 0 of
    True  -> pi/2
    False -> acos $ dot x y / (norm_2 x * norm_2 y) 

-- | Bearing of first point to second, relative to x axis.
bearing :: Point -> Point -> Radians Double
bearing a b = Radians $ atan2 y x
  where (V2 x y) = b ^-^ a

bearingGame :: Point -> Point -> GameAngle
bearingGame a b = toGameAngle (bearing a b)

calculateAngleBetween :: (HasPos t Point, HasPos u Point) => t -> u -> Double
calculateAngleBetween t u = vectorAngle (t ^. pos) (u ^. pos)

-- self, target, min_distance
closestPointTo :: Point -> Circle -> Point
closestPointTo x c@(Circle y r) =
  case norm_2 (a - x) < norm_2 (b - x) of
    True  -> a
    False -> b
  where (a,b) = maybe (x,x) id $ intersectSegmentCircle (Line x y) c
