{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
module Halite.Networking where

import Halite.Types
import Data.Attoparsec.Text
import Control.Monad

import Numeric.LinearAlgebra (Vector(..), vector)

---------- Useful combinators

lineOf :: Parser a -> Parser a
lineOf p = do
  r <- p
  endOfLine
  return r

-- | Parse N 'sep'-separated parsers
sepByN :: Integer -> Parser () -> Parser a -> Parser [a]
sepByN n sep p
  | n <= 0 = return []
  | otherwise = liftM2 (:) p rest
    where rest = replicateM (fromInteger $ n - 1) (sep >> p)

listOfN :: Integer -> Parser a -> Parser [a]
listOfN n p
  | n <= 0    = return []
  | otherwise = sepByN n skipSpace p

list :: Parser a -> Parser (Integer, [a])
list p = do
  n <- decimal
  case n <= 0 of
    True  -> return (n, [])
    False -> skipSpace >> listOfN n p >>= \r -> return (n, r)

---------- Actual parsers

-- | Parse an identifie
parseUid :: Parser (Id t)
parseUid = Id <$> decimal

-- | Parse player ID, then width and height on own line.
parseHeader :: Parser (Id PlayerID, Integer, Integer)
parseHeader = do
  playerId <- lineOf parseUid
  (width, height) <- lineOf $ do
    w  <- decimal
    skipSpace
    h  <- decimal
    return (w, h)
  return (playerId, width, height)

parseInit :: Parser Init
parseInit = do
  (_playerId, _width, _height) <- parseHeader
  _gameMap <- lineOf (skipSpace >> parseGameMap)
  return Init{..}

parseGameMap :: Parser GameMap
parseGameMap = do
  (_numPlayers, _players) <- list player 
  skipSpace
  (_numPlanets, _planets) <- list planet
  return GameMap{..}

player :: Parser Player
player = do
  _uid <- parseUid
  skipSpace
  (_numShips, _ships) <- list ship
  return Player{..}

parseDockingInfo :: Parser DockingInfo
parseDockingInfo = do
  status <- decimal
  skipSpace
  pid <- decimal
  skipSpace
  progress <- decimal
  return $ case status of
             0 -> Undocked
             1 -> Docking   (Id pid) progress
             2 -> Docked    (Id pid)
             3 -> Undocking (Id pid) progress

parsePos :: Parser Point
parsePos = do
  x <- double
  skipSpace
  y <- double
  return $ vector [x, y]

ship :: Parser Ship
ship = do
  _uid <- parseUid
  skipSpace
  _pos <- parsePos
  skipSpace
  _health <- decimal
  skipSpace
  _ <- parsePos -- velocity (deprecated)
  skipSpace
  _dockingInfo <- parseDockingInfo
  skipSpace
  _weaponCooldown <- decimal
  return Ship{..}

parseOwner :: Parser (Maybe (Id PlayerID))
parseOwner = do
  owned <- decimal :: Parser Integer
  skipSpace
  owner <- decimal
  case owned of
    0 -> return Nothing
    _ -> return $ Just (Id owner)

planet :: Parser Planet
planet = do
  _uid <- parseUid
  skipSpace
  _pos <- parsePos
  skipSpace
  _health <- decimal
  skipSpace
  _radius <- double
  skipSpace
  _dockingSpots <- decimal
  skipSpace
  _currentProduction <- decimal
  skipSpace
  _remainingProduction <- decimal
  skipSpace
  _owner <- parseOwner
  skipSpace
  (_numDockedShips, _dockedShips) <- list parseUid
  return Planet{..}
