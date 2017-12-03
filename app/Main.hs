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

-- | Main loop, wrapping settler bot.
mainLoop :: Init -> GameMap -> IO ()
mainLoop init gameMap = do
  let commands = settler init gameMap

  -- Write commands
  forM_ commands $ \c -> do
    putStr (encodeCommand c ++ " ")
  putStrLn ""

  gameMap <- undefined
  mainLoop init gameMap

{-main :: IO ()-}
{-main = do-}
  {-init <- undefined -- TODO: run parser on stdin?-}
  {-mainLoop init (init ^. gameMap)-}

main = print 0
