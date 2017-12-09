{-# LANGUAGE FlexibleContexts #-}
module Main where

import Halite

import Control.Monad
import Control.Lens
import Data.Maybe

-- | Try 
dockWith :: GameMap -> Ship -> Planet -> Maybe Command
dockWith gameMap ship planet = do
  owner <- planet ^. owner
  case canDock ship planet of
    True  -> Just $ Dock (ship ^. uid) (planet ^. uid)
    False -> navigateTo gameMap 7 ship target
  where target = closestDockingPoint (ship^.pos) planet

-- | Process undocked ships
processShip :: GameMap -> Ship -> Maybe Command
processShip gameMap ship =
  msum $ map (dockWith gameMap ship) (gameMap^.planets)

-- | Settler bot, without any monadic machinery
settler :: Init -> GameMap -> [Command]
settler init gameMap =
  catMaybes $ map (processShip gameMap) undockedShips
  where
    myId     = init ^. playerId
    myPlayer = (gameMap ^. players) !! fromIntegral (unId myId)
    myShips  = myPlayer ^. ships
    undockedShips = filter isUndocked myShips
    isUndocked s = s^.dockingInfo == Undocked

type Bot = Init -> GameMap -> [Command]

-- | run a bot in a loop, given an initial msg and game map
botLoop :: Bot -> Init -> GameMap -> IO ()
botLoop bot init gameMap = do
  let commands = bot init gameMap

  -- Write commands
  forM_ commands $ \c -> do
    putStr (encodeCommand c ++ " ")
  putStrLn ""

  gameMap <- undefined
  runBot bot init gameMap

runBot :: Bot -> IO ()
runBot = do
  init <- undefined -- TODO: run parser on stdin?
  mainLoop init (init ^. gameMap)

main = print 0
