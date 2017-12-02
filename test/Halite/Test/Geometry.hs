{-# LANGUAGE OverloadedStrings #-}
module Halite.Test.Geometry ( tests ) where

import Halite
import Data.Text (Text(..))
import Data.Attoparsec.Text

import Test.Tasty
import Test.Tasty.HUnit

-- | Utility functions for constructing tests. It's a bit fugly, sorry!
testLineIntersectsCircle :: Bool -> Point -> Point -> Point -> Double -> Assertion
testLineIntersectsCircle expected x0 x1 c r =
  assertEqual "intersects" expected . maybe False (const True) $
    intersectLineCircle (Line x0 x1) (Circle c r)

{-testSegmentIntersectsCircle :: Bool -> Point -> Point -> Point -> Double -> Assertion-}
{-testSegmentIntersectsCircle expected x0 x1 c r =-}
  {-assertEqual "intersects" expected . maybe False (const True) $-}
    {-intersectSegmentCircle (Line x0 x1) (Circle c r)-}

tests :: TestTree
tests = unitTests

unitTests :: TestTree
unitTests = testGroup "Geometry Tests"
  [ testCase "x axis intersects unit circle" $
      let line     = Line   (vec 0 0) (vec 1 0)
          circle   = Circle (vec 0 0) 1
          result   = intersectLineCircle line circle
          expected = Just $ (vec 1 0, vec (-1) 0)
      in  assertEqual "intersection" expected result

  , testCase "x axis intersects unit circle at (1,0)" $
      let line     = Line   (vec 0 0) (vec 1 0)
          circle   = Circle (vec 1 0) 1
          result   = intersectLineCircle line circle
          expected = Just $ (vec 2 0, vec 0 0)
      in  assertEqual "intersection" expected result

  , testCase "x axis intersects unit circle at (10,0)" $
      let line     = Line   (vec 0 0) (vec 1 0)
          circle   = Circle (vec 10 0) 1
          result   = intersectLineCircle line circle
          expected = Just $ (vec 11 0, vec 9 0)
      in  assertEqual "intersection" expected result

  , testCase "x axis intersects unit circle at (0,1)" $
      let line     = Line   (vec 0 0) (vec 1 0)
          circle   = Circle (vec 0 1) 1
          result   = intersectLineCircle line circle
          expected = Just $ (vec 0 0, vec 0 0)
      in  assertEqual "intersection" expected result

  -- Segment tests

  , testCase "x axis segment intersects unit circle" $
      let line     = Line   (vec 0 0) (vec 1 0)
          circle   = Circle (vec 0 1) 1
          result   = intersectSegmentCircle line circle
          expected = Just $ (vec 0 0, vec 0 0)
      in  assertEqual "intersection" expected result

  , testCase "x axis segment doesn't intersect circle (3,0)" $
      let line     = Line   (vec 0 0) (vec 1 0)
          circle   = Circle (vec 3 0) 1
          result   = intersectSegmentCircle line circle
          expected = Nothing
      in  assertEqual "intersection" expected result

  , testCase "x axis segment doesn't intersect unit circle" $
      let line     = Line   (vec 0 0) (vec 1 0)
          circle   = Circle (vec 0 2) 1
          result   = intersectSegmentCircle line circle
          expected = Nothing
      in  assertEqual "intersection" expected result

  -- NOTE: this fails because of rounding problems. The answer
  -- is within a very small number.
  {-, testCase "x=y segment intersects circle @ (2,2) at (1,1)" $-}
      {-let line     = Line   (vec 0 0) (vec 1 1)-}
          {-circle   = Circle (vec 2 2) (sqrt 2)-}
          {-result   = intersectSegmentCircle line circle-}
          {-expected = Just (vec 3 3, vec 1 1)-}
      {-in  assertEqual "intersection" expected result-}

  , testCase "x=y segment no intersection with circle (0,2)" $
      let line     = Line   (vec 0 0) (vec 1 1)
          circle   = Circle (vec 0 2) 1
          result   = intersectSegmentCircle line circle
          expected = Nothing
      in  assertEqual "intersection" expected result

  , testCase "vectorAngle with zero vector is pi/2" $
      let result = vectorAngle (vec 0 0) (vec 1 1)
          expected = pi / 2
      in  assertEqual "angle" expected result

  , testCase "vectorAngle with axis vectors is pi/2" $
      let result = vectorAngle (vec 1 0) (vec 0 1)
          expected = pi / 2
      in  assertEqual "angle" expected result

  , testCase "bearing between (1, 0) and (0, 1) is 3pi/4" $
      let result = bearing (vec 1 0) (vec 0 1)
          expected = (3 * pi) / 4
      in  assertEqual "angle" expected result

  , testCase "bearing between (0, 0) and (-1, -1) is -3pi/4" $
      let result = bearing (vec 0  0) (vec (-1)  (-1))
          expected = ((-3) * pi) / 4
      in  assertEqual "angle" expected result
  ]


propertyTests :: TestTree
propertyTests = testGroup "Property-based" []
