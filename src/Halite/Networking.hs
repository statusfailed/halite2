{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Halite.Networking where

import Halite.Types
import Halite.Parser

import Data.Attoparsec.Text
import Data.Text.IO as Text (getLine)

import System.IO

parseLine :: Show a => Parser a -> IO a
parseLine p = do
  line <- Text.getLine
  case parseOnly p line of
    Left err -> error err
    Right r  -> return r


-- | Get the 'Header' message
recvHeader :: IO Header
recvHeader = do
  _playerId <- parseLine parseUid
  (_width, _height) <- parseLine parseWidthHeight
  return Header{..}

recvGameMap :: IO GameMap
recvGameMap = parseLine parseGameMap
