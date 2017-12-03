{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
module Halite.Starter where

import Halite.Types
import Halite.Commands
import Halite.Game

import Linear.Metric
import Linear.V2
import Control.Lens

-- self, target, map, speed
navigateTo'
  :: GameMap
  -> Integer -- ^ Max corrections
  -> Integer -- ^ Speed
  -> Ship    -- ^ Ship to move
  -> Point   -- ^ Target to move to
  -> Maybe Command
navigateTo' gameMap 0 speed ship target = Nothing
navigateTo' gameMap maxCorrections speed ship target =
  case obstacles of
    [] -> Just $ Move (ship ^. uid) speed (toGameAngle angle)
    _  -> navigateTo' gameMap maxCorrections' speed ship target
  where
    dist  = distance target (ship^.pos)
    angle = bearing (ship^.pos) target
    obstacles = obstaclesBetween gameMap (ship^.pos) newTarget
    maxCorrections' = maxCorrections - 1
    angularStep = Radians (pi/180) -- approx 1 degree

    dx = cos (unRadians $ angle + angularStep) * dist
    dy = sin (unRadians $ angle + angularStep) * dist
    newTarget = vec (target^._x + dx) (target^._y + dy)

-- | Navigate a ship to a point on the map.
-- Ported from the Python starter kit
navigateTo
  :: GameMap -- ^ Game state
  -> Integer -- ^ Speed
  -> Ship    -- ^ Ship to move
  -> Point   -- ^ Target to move to
  -> Maybe Command
navigateTo = flip navigateTo' 90
