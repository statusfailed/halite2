module Halite.Spatial where

import Numeric.LinearAlgebra

-- python:
-- return math.sqrt((target.x - self.x) ** 2 + (target.y - self.y) ** 2)
distance :: Vector Double -> Vector Double -> Double
distance x y = norm_2 (x - y)
