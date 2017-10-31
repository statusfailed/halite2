module Halite.Spatial where

import Numeric.LinearAlgebra
import Halite.Types


-- | List of obstacles on the straight line between two points.
obstaclesBetween :: GameMap -> Position -> Position -> [Either Ship Planet]
obstaclesBetween gameMap x y = candidates
  where candidates = map Left (gameMap^..players^..ships) ++ map Right (gameMap^..planets)


navigateTo :: GameMap -> Ship -> Position -> Vector Double
navigateTo = error "todo: navigateTo"
