{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Halite

import Data.Function
import Data.List
import Data.Maybe

import Control.Monad
import Control.Lens
import System.IO

import Control.Monad.State
import Control.Monad.Reader

import Linear

----------------------------------------------------------------------
-- Utils -------------------------------------------------------------
----------------------------------------------------------------------

closestPlanetTo :: HaliteBot s m => Point -> m Planet
closestPlanetTo point = do
  planets <- view (gameMap . planets)
  let planet = minimumBy f planets
      f p1 p2 = compare (g p1 point) (g p2 point)
      g p s   = distance (p ^. pos) point
  return $ minimumBy f planets

dockWithClosestPlanet :: HaliteBot s m => Ship -> m (Maybe Command)
dockWithClosestPlanet ship = do
  planet <- closestPlanetTo (ship ^. pos)
  gm     <- view gameMap
  let target = closestDockingPoint (ship ^. pos) planet
      dist    = distance (ship ^. pos) target
      speed   = 7

  case withinDockingRadius (ship^.pos) planet of
    False -> return $ navigateTo gm speed ship target
    True  -> return . Just $ Dock (ship^.uid) (planet^.uid)

dockWith :: HaliteBot s m => Ship -> Planet -> m (Maybe Command)
dockWith ship planet = do
  gm <- view gameMap
  let target = closestDockingPoint (ship ^. pos) planet
      dist    = distance (ship ^. pos) target
      speed   = 7

  case withinDockingRadius (ship^.pos) planet of
    False -> return $ navigateTo gm speed ship target
    True  -> return . Just $ Dock (ship^.uid) (planet^.uid)

----------------------------------------------------------------------
-- Bots  -------------------------------------------------------------
----------------------------------------------------------------------

-- | Settler bot
settler :: HaliteBot s m => m [Command]
settler = do
  myId        <- view (header . playerId)
  freeShips   <- filter isUndocked <$> myShips
  freePlanets <- filter isUnowned  <$> view (gameMap.planets)
  case freePlanets of
    (p:_) -> catMaybes <$> mapM (flip dockWith p) freeShips
    []    -> return $ []
  where f (Id i) = compare `on` (\x -> unId (x^.uid) `mod` (i+1))

-- | DockFastBot just tries to dock ASAP
dockFastBot :: HaliteBot s m => m [Command]
dockFastBot  = do
  planets <- view (gameMap . planets)
  ships   <- myShips
  cmds    <- mapM dockWithClosestPlanet ships
  return $ catMaybes cmds

-- | GreedyBot tries to dock
greedyBot :: HaliteBot s m => m [Command]
greedyBot  = do
  planets <- view (gameMap . planets)
  ships   <- myShips
  catMaybes <$> mapM shipCmd ships
  where
    -- Each ship docks with the closest planet, or does nothing.
    shipCmd ship = do
      planets <- view (gameMap . planets)
      targets <- sortByDistance ship <$> filterM dockable planets
      case targets of
        []    -> return Nothing
        (p:_) -> dockWith ship p

----------------------------------------------------------------------
-- Plumbing ----------------------------------------------------------
----------------------------------------------------------------------

writeCommands :: [Command] -> IO ()
writeCommands commands = do
  forM_ commands $ \c -> do
    putStr (encodeCommand c ++ " ")
  putStrLn ""


runBotWith :: Header -> GameMap -> s -> Bot s a -> a
runBotWith hdr gm botState bot
  = flip evalState botState $ runReaderT bot (GameState hdr gm)

-- | Run a 'HaliteBot'
runBot :: String -> Bot () [Command] -> IO ()
runBot name bot = do
  hdr <- recvHeader
  gm  <- recvGameMap
  putStrLn name
  loop hdr gm
  where
    loop :: Header -> GameMap -> IO ()
    loop hdr gm = do
      let commands = runBotWith hdr gm () bot
      writeCommands commands
      recvGameMap >>= loop hdr

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  runBot "HaskellTest" greedyBot
