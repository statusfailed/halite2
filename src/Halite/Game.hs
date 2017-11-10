{-# LANGUAGE FlexibleContexts #-}
module Halite.Game where

import Numeric.LinearAlgebra
import Halite.Types
import Halite.Geometry
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

navigateTo :: GameMap -> Ship -> Point -> Vector Double
navigateTo = error "todo: navigateTo"
