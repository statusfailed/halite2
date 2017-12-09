{-# LANGUAGE FlexibleContexts #-}
module Main where

import Halite

import Control.Monad
import Control.Lens
import Data.Maybe
import System.IO

-- | Try to dock with a planet if within its radius.
-- Otherwise, navigate to the closest point to it.
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
botLoop :: GameMap -> Bot -> Header -> IO ()
botLoop gameMap bot header = do
  let commands = bot header gameMap

  -- Write commands
  forM_ commands $ \c -> do
    putStr (encodeCommand c ++ " ")
  putStrLn ""

  -- Loop
  gameMap <- recvGameMap
  botLoop gameMap bot header

-- | Run a bot communicating on stdio
-- TODO: strip any newlines from 'name', otherwise it will break :D
runBot :: String -> Bot -> IO ()
runBot name bot = do
  header <- recvHeader
  gameMap <- recvGameMap
  putStrLn name
  botLoop gameMap bot header

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  runBot "Settler" settler
