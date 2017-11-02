{-# LANGUAGE OverloadedStrings #-}
module Halite.Test.Geometry ( tests ) where

import Halite
import Data.Text (Text(..))
import Data.Attoparsec.Text

import Test.Tasty
import Test.Tasty.HUnit
import Numeric.LinearAlgebra (vector)

-- | Utility functions for constructing tests. It's a bit fugly, sorry!
testLineIntersectsCircle :: Bool -> Point -> Point -> Point -> Double -> Assertion
testLineIntersectsCircle expected x0 x1 c r =
  assertEqual "intersects" expected . maybe False (const True) $
    lineIntersectsCircle (Line x0 x1) (Circle c r)

{-testSegmentIntersectsCircle :: Bool -> Point -> Point -> Point -> Double -> Assertion-}
{-testSegmentIntersectsCircle expected x0 x1 c r =-}
  {-assertEqual "intersects" expected . maybe False (const True) $-}
    {-segmentIntersectsCircle (Line x0 x1) (Circle c r)-}

tests :: TestTree
tests = unitTests

unitTests :: TestTree
unitTests = testGroup "Geometry Tests"
  [ testCase "x axis intersects unit circle" $
      let line     = Line   (vector [0,0]) (vector [1,0])
          circle   = Circle (vector [0,0]) 1
          result   = lineIntersectsCircle line circle
          expected = Just $ (vector [1,0], vector [-1,0])
      in  assertEqual "intersection" expected result

  , testCase "x axis intersects unit circle at (1,0)" $
      let line     = Line   (vector [0,0]) (vector [1,0])
          circle   = Circle (vector [1,0]) 1
          result   = lineIntersectsCircle line circle
          expected = Just $ (vector [2,0], vector [0,0])
      in  assertEqual "intersection" expected result

  , testCase "x axis intersects unit circle at (0,1)" $
      let line     = Line   (vector [0,0]) (vector [1,0])
          circle   = Circle (vector [0,1]) 1
          result   = lineIntersectsCircle line circle
          expected = Just $ (vector [0,0], vector [0,0])
      in  assertEqual "intersection" expected result

  ]


propertyTests :: TestTree
propertyTests = testGroup "Property-based" []
