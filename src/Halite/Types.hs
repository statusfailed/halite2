{-# LANGUAGE DuplicateRecordFields #-}
module Halite.Types where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

-- | A general ID type
data Id t = Id { unId :: Integer }
  deriving(Eq, Ord, Read, Show)

-- types of ID
data PlayerID
data ShipID
data PlanetID

type Point = Vector Double

data Init = Init
  { playerId :: Id PlayerID
  , width    :: Integer
  , height   :: Integer
  , gameMap  :: GameMap
  } deriving(Eq, Ord, Read, Show)

data GameMap = GameMap
  { numPlayers :: Integer
  , players    :: [Player]
  , numPlanets    :: Integer
  , planets    :: [Planet]
  } deriving(Eq, Ord, Read, Show)

data Player = Player
  { uid            :: Id PlayerID
  , numShips      :: Integer
  , ships         :: [Ship]
  } deriving(Eq, Ord, Read, Show)

data Ship = Ship
  { uid :: Id ShipID
  , pos :: Point
  , health :: Integer
  -- velocity is deprecated
  , dockingInfo :: DockingInfo
  , weaponCooldown :: Integer
-- NOTE: lack of docked_planet - this is part of DockingStatus
  } deriving(Eq, Ord, Read, Show)

data DockingInfo
  = Undocked
  | Docking   (Id PlanetID) Integer -- ^ Planet ID and docking progress
  | Docked    (Id PlanetID)
  | Undocking (Id PlanetID) Integer -- ^ Planet ID and undocking progress
  deriving(Eq, Ord, Read, Show)

data Planet = Planet
  { uid :: Id PlanetID
  , pos :: Point
  , health :: Integer
  , radius :: Double
  , dockingSpots :: Integer
  , currentProduction :: Integer
  , remainingProduction :: Integer
  , owner :: Maybe (Id PlayerID) -- ^ owned 0/1, owner int
  , numDockedShips :: Integer
  , dockedShips :: [Id ShipID]
  } deriving(Eq, Ord, Read, Show)
