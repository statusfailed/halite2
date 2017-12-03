{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Halite.Angle where

newtype Radians a = Radians { unRadians :: a }
  deriving(Eq, Ord, Read, Show, Num, Fractional, Floating)

-- | Degrees can be negative
newtype Degrees a = Degrees { unDegrees :: a }
  deriving(Eq, Ord, Read, Show, Num, Fractional, Floating)

newtype GameAngle = GameAngle { unGameAngle :: Integer }
  deriving(Eq, Ord, Read, Show, Num)

toDegrees :: Radians Double  -> Degrees Double
toDegrees (Radians a) = Degrees (radiansToDegrees a)

toGameAngle :: Radians Double -> GameAngle
toGameAngle (Radians a) = GameAngle (radiansToGame a)


radiansToDegrees :: Double -> Double
radiansToDegrees theta = 360 * theta / (2*pi)

radiansToGame :: Double -> Integer
radiansToGame = (`mod` 360) . round . radiansToDegrees
