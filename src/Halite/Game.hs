{-# LANGUAGE FlexibleContexts #-}
module Halite.Game where

import Linear (dot, V2(..), (^-^))

import Halite.Types
import Halite.Geometry
import Halite.Commands

import Linear
import Control.Lens
import Control.Monad
import Data.List
import Data.Function

-- | Intersection between a 'Line' and an 'Entity'
-- adds a fudge factor of 0.5
intersectLineEntity :: Entity t => Line -> t -> Maybe (Point, Point)
intersectLineEntity line =
  intersectSegmentCircle line . (\x -> Circle (x ^. pos) (x ^. radius + 0.5))

-- | List of obstacles on the straight line between two points.
obstaclesBetween :: GameMap -> Point -> Point -> [Either Planet Ship]
obstaclesBetween gameMap x y = allShips ++ allPlanets
  where
    f :: Entity t => t -> Bool
    f = maybe False (const True) . intersectLineEntity (Line x y)

    allShips   = map Right . filter f $ (gameMap ^. players . traverse . ships)
    allPlanets = map Left  . filter f $ (gameMap ^. planets)

obstaclesBetweenShip :: GameMap -> Ship -> Point -> [Either Planet Ship]
obstaclesBetweenShip gameMap ship y = result
  where result = filter (/= Right ship) . obstaclesBetween gameMap (ship^.pos) $ y

vectorAngle :: Point -> Point -> Double
vectorAngle x y = -- note: zero vector angle is defined as pi/2 radians
  case sum x == 0 || sum y == 0 of
    True  -> pi/2
    False -> acos $ dot x y / (norm_2 x * norm_2 y)

-- | Bearing of first point to second, relative to x axis.
bearing :: Point -> Point -> Radians Double
bearing a b = Radians $ atan2 y x
  where (V2 x y) = b ^-^ a

-- | Get a 'GameAngle' between two 'Point's
bearingGame :: Point -> Point -> GameAngle
bearingGame a b = toGameAngle (bearing a b)

-- | Closest docking point to a planet from a given point.
-- TODO: why does this only work when adding both dockRadius and fudge factor!?!?
-- Ships should be too far away to dock!
closestDockingPoint :: Point -> Planet -> Point
closestDockingPoint x planet = closestPointTo x circle
  where
    circle = Circle (planet^.pos) (planet^.radius + dockRadius + 3)

-- | Can a ship dock with a planet?
withinDockingRadius :: Point -> Planet -> Bool
withinDockingRadius point planet =
  distance point (planet^.pos) < planet^.radius + dockRadius

-- | A list of ships owned by the running bot
myShips :: HaliteBot s m => m [Ship]
myShips = do
  myId <- view (header . playerId)
  view (gameMap . players . to (!! fromIntegral (unId myId)) . ships)

isUndocked :: Ship -> Bool
isUndocked ship = ship ^. dockingInfo == Undocked

isUnowned :: Planet -> Bool
isUnowned planet = planet ^. owner == Nothing

ownedByMe :: HaliteBot s m => Planet -> m Bool
ownedByMe planet = do
  myId <- view (header . playerId)
  return $ planet ^. owner == Just (myId)

-- | Returns 'True' if it's possible to dock with this planet when close enough.
dockable :: HaliteBot s m => Planet -> m Bool
dockable planet = do
  mine <- ownedByMe planet
  let hasSpots = planet^.dockingSpots > planet^.numDockedShips
  return (isUnowned planet || (mine && hasSpots))

-- | returns 'True' if ship can dock with planet right now
canDock :: HaliteBot s m => Ship -> Planet -> m Bool
canDock ship planet = do
  d <- dockable planet
  return (d && withinDockingRadius (ship ^. pos) planet)

sortByDistance :: Ship -> [Planet] -> [Planet]
sortByDistance ship = sortBy comparator
  where
    comparator = compare `on` score ship
    score ship planet = distance (planet^.pos) (ship^.pos)
