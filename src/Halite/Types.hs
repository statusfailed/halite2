{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Halite.Types
  ( module Halite.Types
  , module Halite.Angle
  )where

import Halite.Angle
import Linear.V2
import Control.Lens
import Control.Lens.Prism

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

-- | A 'HaliteBot' is execution state for a bot
class
  ( Monad m
  , MonadReader GameState m
  , MonadState  botState m
  ) => HaliteBot botState m

instance HaliteBot s (ReaderT GameState (State s))

type Bot s a = ReaderT GameState (State s) a

-- | Dock radius constant
dockRadius :: Double
dockRadius = 4.0

shipRadius :: Double
shipRadius = 1.0

-- | A general ID type
newtype Id t = Id { unId :: Integer }
  deriving(Eq, Ord, Read, Show)

-- types of ID
data PlayerID
data ShipID
data PlanetID

type Vector = V2
type Point  = Vector Double

-- | Wrapper so we can easily change the underlying type later
vec :: Double -> Double -> Point
vec = Linear.V2.V2

data Header = Header
  { _playerId :: Id PlayerID
  , _width    :: Integer
  , _height   :: Integer
  } deriving(Eq, Ord, Read, Show)

data GameMap = GameMap
  { _numPlayers :: Integer
  , _players    :: [Player]
  , _numPlanets    :: Integer
  , _planets    :: [Planet]
  } deriving(Eq, Ord, Read, Show)

data GameState = GameState
  { _header  :: Header
  , _gameMap :: GameMap
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
  , _radius :: Double
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
makeFieldsNoPrefix ''Header
makeFieldsNoPrefix ''GameMap
makeFieldsNoPrefix ''Player
makeFieldsNoPrefix ''Ship
makeFieldsNoPrefix ''DockingInfo
makeFieldsNoPrefix ''Planet
makeFieldsNoPrefix ''GameState

-- | Define Entities as things with a Uid, Position, and Health.
class (HasPos t Point, HasRadius t Double, HasHealth t Integer) => Entity t
instance Entity Ship
instance Entity Planet

-- | Useful instance to pick between Either Planet Ship
-- TODO: how do we lens over both fields at once?
{-instance (Entity l, Entity r) => HasPos (Either l r) where-}
  {-pos = _Left.pos-}

{-instance (Entity l, Entity r) => HasHealth (Either l r) where-}
  {-pos = _Left.health-}
