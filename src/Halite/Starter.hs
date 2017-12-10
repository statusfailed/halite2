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

-- | navigate to, with a max number of corrections
-- TODO: remove target from obstacle entities
navigateTo'
  :: GameMap
  -> Integer -- ^ Max corrections
  -> Integer -- ^ Speed
  -> Ship    -- ^ Ship to move
  -> Point   -- ^ Target to move to
  -> Maybe Command
navigateTo' gameMap 0 speed ship target = Nothing
navigateTo' gameMap maxCorrections speed ship target =
  case obstaclesBetweenShip gameMap ship target of
    [] -> Just $ Move (ship ^. uid) trueSpeed (toGameAngle angle)
    _  -> navigateTo' gameMap (maxCorrections - 1) speed ship newTarget
  where
    dist  = distance target (ship^.pos)
    angle = bearing (ship^.pos) target
    angularStep = Radians (pi/180) -- approx 1 degree
    trueSpeed = if dist >= fromIntegral speed then speed else floor dist

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
