{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
module Halite.Types where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Control.Lens
import Control.Lens.Prism

-- | A general ID type
data Id t = Id { unId :: Integer }
  deriving(Eq, Ord, Read, Show)

-- types of ID
data PlayerID
data ShipID
data PlanetID

type Point = Vector Double

data Init = Init
  { _playerId :: Id PlayerID
  , _width    :: Integer
  , _height   :: Integer
  , _gameMap  :: GameMap
  } deriving(Eq, Ord, Read, Show)

data GameMap = GameMap
  { _numPlayers :: Integer
  , _players    :: [Player]
  , _numPlanets    :: Integer
  , _planets    :: [Planet]
  } deriving(Eq, Ord, Read, Show)

data Player = Player
  { _uid            :: Id PlayerID
  , _numShips      :: Integer
  , _ships         :: [Ship]
  } deriving(Eq, Ord, Read, Show)

data Ship = Ship
  { _uid :: Id ShipID
  , _pos :: Point
  , _health :: Integer
  -- _velocity is deprecated
  , _dockingInfo :: DockingInfo
  , _weaponCooldown :: Integer
-- NOTE: lack of docked_planet - this is part of DockingStatus
  } deriving(Eq, Ord, Read, Show)

data DockingInfo
  = Undocked
  | Docking   (Id PlanetID) Integer -- ^ Planet ID and docking progress
  | Docked    (Id PlanetID)
  | Undocking (Id PlanetID) Integer -- ^ Planet ID and undocking progress
  deriving(Eq, Ord, Read, Show)

data Planet = Planet
  { _uid :: Id PlanetID
  , _pos :: Point
  , _health :: Integer
  , _radius :: Double
  , _dockingSpots :: Integer
  , _currentProduction :: Integer
  , _remainingProduction :: Integer
  , _owner :: Maybe (Id PlayerID) -- ^ owned 0/1, owner int
  , _numDockedShips :: Integer
  , _dockedShips :: [Id ShipID]
  } deriving(Eq, Ord, Read, Show)

-- Make lenses!
makeFieldsNoPrefix ''Init
makeFieldsNoPrefix ''GameMap
makeFieldsNoPrefix ''Player
makeFieldsNoPrefix ''Ship
makeFieldsNoPrefix ''DockingInfo
makeFieldsNoPrefix ''Planet

-- | Define Entities as things with a Uid, Position, and Health.
class (HasPos Point t, HasHealth Integer t) => Entity t

-- | Useful instance to pick between Either Planet Ship
-- TODO: how do we lens over both fields at once?
{-instance (Entity l, Entity r) => HasPos (Either l r) where-}
  {-pos = _Left.pos-}

{-instance (Entity l, Entity r) => HasHealth (Either l r) where-}
  {-pos = _Left.health-}
