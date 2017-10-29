{-# LANGUAGE RecordWildcards        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
module Halite.Networking where

import Halite.Types

parseUid :: Parser (Id t)
parseUid = Id <$> integer

init :: Parser Init
init = liftM4 Init
  uid
  integer
  integer
  parseGameMap

gameMap :: Parser GameMap
gameMap = do
  numPlayers <- integer
  players <- replicateM numPlayers player
  numPlanets <- integer
  planets <- replicateM numPlanets planet
  return GameMap{..}

player :: Parser Player
player = do
  uid <- parseUid
  numShips <- integer
  ships <- replicateM numShips ship
  return Player{..}

parseDockingInfo :: Parser DockingInfo
parseDockingInfo = do
  status <- integer
  pid    <- integer
  case status of
    0 -> Undocked
    1 -> Docking   (Id pid)
    2 -> Docked    (Id pid)
    3 -> Undocking (Id pid)

parsePos :: Parser (Double, Double)
parsePos = liftM2 (,) double double

ship :: Parser Ship
ship = do
  uid <- parseUid
  pos <- parsePos
  health <- integer
  _ <- replicateM 2 double -- velocity (deprecated)
  dockingInfo <- parseDockingInfo
  return Ship{..}

parseOwner :: Maybe (Id PlayerID)
parseOwner = do
  owned <- integer
  owner <- integer
  case owned of
    0 -> Nothing
    _ -> Just (Id owner)

planet :: Parser Planet
planet = do
  uid <- parseUid
  pos <- parsePos
  health <- integer
  radius <- integer
  dockingSpots <- integer
  currentProduction <- integer
  remainingProduction <- integer
  owner <- parseOwner
  numDockedShips <- integer
  dockedShips <- replicateM numDockedShips integer
  return Planet{..}
