{-# LANGUAGE OverloadedStrings #-}
module Halite.Test.Geometry ( tests ) where

import Halite
import Data.Text (Text(..))
import Data.Attoparsec.Text

import Test.Tasty
import Test.Tasty.HUnit
import Numeric.LinearAlgebra (vector)

-- | Utility function for constructing tests. It's a bit fugly, sorry!
testLineIntersectsCircle :: Bool -> Point -> Point -> Point -> Double -> Assertion
testLineIntersectsCircle expected x0 x1 c r =
  assertEqual "intersects" expected . maybe False (const True) $
    lineIntersectsCircle (Line x0 x1) (Circle c r)

tests = testGroup "Geometry Tests"
  [ testCase "lineIntersectsCircle 1" $
      testLineIntersectsCircle True  (vector [0,0]) (vector [2,2]) (vector [1,1]) 0.5

  , testCase "lineIntersectsCircle 2" $
      testLineIntersectsCircle False (vector [0,0]) (vector [1,0]) (vector [10,10]) 0.5 

  -- Test unit circle intersects line y = 0 at (1,0) and (-1,0)
  , testCase "lineIntersectsCircle 3" $
      assertEqual "unity" (Just (vector [1.0,0.0], vector[-1.0,0.0])) $ lineIntersectsCircle (Line (vector [0,0]) (vector [1,0]) ) (Circle (vector [0,0]) 1)
  ]
