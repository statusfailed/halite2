{-# LANGUAGE DuplicateRecordFields #-}
module Halite.Commands where

import Halite.Types

data Command
  = Move   (Id ShipID) Integer GameAngle
  -- ^ Ship ID, speed, angle (degrees)
  | Dock   (Id ShipID) (Id PlanetID)
  | Undock (Id ShipID)
  deriving(Eq, Ord, Read, Show)

encodeCommand :: Command -> String
encodeCommand (Move (Id i) speed angle) = "t " ++ show i ++ " " ++ show speed ++ " " ++ (show . unGameAngle) angle
encodeCommand (Dock (Id ship) (Id planet)) = "d " ++ show ship ++ " " ++ show planet
encodeCommand (Undock (Id ship)) = "u " ++ show ship
