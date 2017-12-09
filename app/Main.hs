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
settler :: Header -> GameMap -> [Command]
settler header gameMap =
  catMaybes $ map (processShip gameMap) undockedShips
  where
    myId     = header ^. playerId
    myPlayer = (gameMap ^. players) !! fromIntegral (unId myId)
    myShips  = myPlayer ^. ships
    undockedShips = filter isUndocked myShips
    isUndocked s = s^.dockingInfo == Undocked

type Bot = Header -> GameMap -> [Command]

-- | run a bot in a loop, given an initial msg and game map
botLoop :: Bot -> Header -> IO ()
botLoop bot header = do
  gameMap <- recvGameMap
  let commands = bot header gameMap

  -- Write commands
  forM_ commands $ \c -> do
    putStr (encodeCommand c ++ " ")
  putStrLn ""

  -- Loop
  botLoop bot header

-- | Run a bot communicating on stdio
runBot :: Bot -> IO ()
runBot bot = do
  header <- recvHeader
  botLoop bot header

main :: IO ()
main = runBot settler
