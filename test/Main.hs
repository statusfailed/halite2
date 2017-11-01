{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.SmallCheck
import Test.Tasty.HUnit

import Halite
import qualified Halite.Test.Parser as Parser
import qualified Halite.Test.Geometry as Geometry

import Data.Text (Text(..))
import Data.Attoparsec.Text
import Numeric.LinearAlgebra (vector)

-- see example code:
-- https://github.com/ocharles/blog/blob/master/code/2013-12-03-tasty-example.hs

-- | Test runner
main :: IO ()
main = defaultMain $
  testGroup "Tests" [ Parser.tests, Geometry.tests ]
