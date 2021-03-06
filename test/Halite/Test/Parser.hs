{-# LANGUAGE OverloadedStrings #-}
module Halite.Test.Parser ( tests ) where

import Halite
import Data.Text (Text(..))
import Data.Attoparsec.Text

import Test.Tasty
import Test.Tasty.HUnit

assertDoneWith :: Show r => (r -> IO a) -> Parser r -> Text -> Assertion
assertDoneWith f p t = do
  case parse p t of
    Fail _ contexts err -> assertFailure err
    Partial f -> assertFailure $ "failed: partial parse: " ++ show (f "")
    Done _ r -> putStr " succeeded with result: " >> f r >> print r

assertDone :: Show r => Parser r -> Text -> Assertion
assertDone = assertDoneWith (const $ return ())

tests :: TestTree
tests = testGroup "Parser Tests"
  [ testCase "parse testUid" $ assertDone (lineOf parseUid) "0\n"

  , testCase "parse sepByN decimal" $
      assertDoneWith (assertEqual "result" [1,2,3,4,5])
        (lineOf $ sepByN 5 skipSpace decimal) "1 2 3 4 5\n"

  , testCase "parse sepByN empty" $
      assertDoneWith (assertEqual "result" [])
        (lineOf $ sepByN 0 skipSpace decimal) "\n"

  , testCase "parse list decimal" $
      assertDoneWith (assertEqual "result" (5,[1,2,3,4,5]))
        (lineOf $ list decimal) "5 1 2 3 4 5\n"

  , testCase "parse list empty" $
      assertDoneWith (assertEqual "result" (0, []))
        (lineOf $ list decimal) "0\n"

  , testCase "parse header" $ assertDone parseHeader "0\n240 160\n"

  , testCase "parse docking info Undocked" $
      assertDoneWith (assertEqual "pdi" Undocked) (lineOf parseDockingInfo) "0 0 0\n"

  , testCase "parse docking info Docking" $
      assertDoneWith (assertEqual "pdi" (Docking (Id 314) 0)) (lineOf parseDockingInfo) "1 314 0\n"

  , testCase "parse pos" $
      assertDoneWith (assertEqual "pos" $ vec 60.0 80.0) (lineOf parsePos) "60.0000 80.0000\n"

  , testCase "parse ship" $
      assertDone (lineOf ship) "0 60.0000 80.0000 255 0.0000 0.0000 0 0 0 0\n"

  , testCase "parse player" $
      assertDone (lineOf player) examplePlayer

  , testCase "parse planet" $
      assertDone (lineOf planet) "11 25.1413 72.0055 1150 4.5129 2 0 649 0 0 0\n"

  , testCase "parse header"$ assertDone parseHeader exampleInit
  , testCase "parse gameMap"$ assertDone parseGameMap exampleGameMap
  ]

-- Some ugly test data

exampleInit = "\
   \1\n\
   \240 160\n"

exampleGameMap = "\
   \ 2 0 3 0 60.0000 80.0000 255 0.0000 0.0000 0 0 0 0 1 60.0000 77.0000 255 0.0000 0.0000 0 0 0 0 2 60.0000 83.0000 255 0.0000 0.0000 0 0 0 0 1 3 3 180.0000 80.0000 255 0.0000 0.0000 0 0 0 0 4 180.0000 77.0000 255 0.0000 0.0000 0 0 0 0 5 180.0000 83.0000 255 0.0000 0.0000 0 0 0 0 12 0 130.7715 90.7715 1814 7.1166 3 0 1024 0 0 0 1 109.2285 90.7715 1814 7.1166 3 0 1024 0 0 0 2 109.2285 69.2285 1814 7.1166 3 0 1024 0 0 0 3 130.7715 69.2285 1814 7.1166 3 0 1024 0 0 0 4 85.6058 18.5976 3224 12.6469 5 0 1821 0 0 0 5 165.4537 33.5377 3224 12.6469 5 0 1821 0 0 0 6 154.3942 141.4024 3224 12.6469 5 0 1821 0 0 0 7 74.5463 126.4623 3224 12.6469 5 0 1821 0 0 0 8 134.7591 28.6182 1150 4.5129 2 0 649 0 0 0 9 214.8587 87.9945 1150 4.5129 2 0 649 0 0 0 10 105.2409 131.3818 1150 4.5129 2 0 649 0 0 0 11 25.1413 72.0055 1150 4.5129 2 0 649 0 0 0\n"

examplePlayer = "0 3 0 60.0000 80.0000 255 0.0000 0.0000 0 0 0 0 1 60.0000 77.0000 255 0.0000 0.0000 0 0 0 0 2 60.0000 83.0000 255 0.0000 0.0000 0 0 0 0\n"
