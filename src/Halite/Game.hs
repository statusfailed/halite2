{-# LANGUAGE FlexibleContexts #-}
module Halite.Game where

import Numeric.LinearAlgebra
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
  case sumElements x == 0 || sumElements y == 0 of
    True  -> pi/2
    False -> acos $ dot x y / (norm_2 x * norm_2 y) 

-- | Bearing of first point to second, relative to x axis.
bearing :: Point -> Point -> Double
bearing a b =
  case size c of
    2 -> atan2 (atIndex c 1) (atIndex c 0)
    _ -> error "bearing used on non-2D vector" -- gross.
  where c = b - a

bearingDegrees :: Point -> Point -> Integer
bearingDegrees a b = round (bearing a b / (2 * pi)) `mod` 360

calculateAngleBetween :: (HasPos t Point, HasPos u Point) => t -> u -> Double
calculateAngleBetween t u = vectorAngle (t ^. pos) (u ^. pos)

-- self, target, min_distance
closestPointTo :: Point -> Circle -> Point
closestPointTo x c@(Circle y r) =
  case norm_2 (a - x) < norm_2 (b - x) of
    True  -> a
    False -> b
  where (a,b) = maybe (x,x) id $ intersectSegmentCircle (Line x y) c

radiansToDegrees :: Double -> Double
radiansToDegrees theta = 360 * theta / (2*pi)

radiansToGame :: Double -> Integer
radiansToGame = (`mod` 360) . round . radiansToDegrees

-- self, target, map, speed
{-navigateTo-}
  {-:: GameMap-}
  {--> Integer -- ^ Speed-}
  {--> Ship    -- ^ Ship to move-}
  {--> Point   -- ^ Target to move to-}
  {--> Maybe Command-}
{-navigateTo gameMap speed ship target =-}
  {-case obstacles of-}
    {-[] -> Move (ship ^. id) -}
    {-_  -> navigateTo gameMap speed ship target'-}
  {-where-}
    {-angle     = vectorAngle (ship ^. pos) target-}
    {-obstacles = obstaclesBetween (ship ^. pos) target-}
    {-target'   = undefined-}
