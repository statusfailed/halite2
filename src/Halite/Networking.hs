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
  (playerId, width, height) <- parseHeader
  gameMap <- lineOf (skipSpace >> parseGameMap)
  return Init{..}

parseGameMap :: Parser GameMap
parseGameMap = do
  (numPlayers, players) <- list player 
  skipSpace
  (numPlanets, planets) <- list planet
  return GameMap{..}

player :: Parser Player
player = do
  uid <- parseUid
  skipSpace
  (numShips, ships) <- list ship
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

parsePos :: Parser Position
parsePos = do
  x <- double
  skipSpace
  y <- double
  return $ vector [x, y]

ship :: Parser Ship
ship = do
  uid <- parseUid
  skipSpace
  pos <- parsePos
  skipSpace
  health <- decimal
  skipSpace
  _ <- parsePos -- velocity (deprecated)
  skipSpace
  dockingInfo <- parseDockingInfo
  skipSpace
  weaponCooldown <- decimal
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
  uid <- parseUid
  skipSpace
  pos <- parsePos
  skipSpace
  health <- decimal
  skipSpace
  radius <- double
  skipSpace
  dockingSpots <- decimal
  skipSpace
  currentProduction <- decimal
  skipSpace
  remainingProduction <- decimal
  skipSpace
  owner <- parseOwner
  skipSpace
  (numDockedShips, dockedShips) <- list parseUid
  return Planet{..}
