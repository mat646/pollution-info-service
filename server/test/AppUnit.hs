module AppUnit where

import Test.HUnit
import App
import Survey
import Table
import Station
import Index
import System.IO.Unsafe

main :: IO Counts
main = runTestTT tests

tests = TestList [TestLabel "countAvgTest" countAvgTest, TestLabel "getLevelNameTest" getLevelNameTest]

countAvgTest :: Test
countAvgTest = TestCase $
  assertEqual "Should return 4" 4
    (computeAvg [Survey{Survey.date="12.12.12",Survey.value=Just(2)},Survey{Survey.date="13.12.12",Survey.value=Just(6)}])

getLevelNameTest :: Test
getLevelNameTest = TestCase $
  assertEqual "xd" "Bardzo dobry"
    (unsafePerformIO (getLevelName (return Station { Station.id = 402, stCalcDate = Just "2018-02-06 14:20:46",
      Station.stIndexLevel = Just (Index { Index.id = Just 1, Index.indexLevelName = Just "Dobry"}),
      stSourceDataDate = Just "2018-02-06 13:00:00", so2CalcDate = Just "2018-02-06 14:20:46",
      Station.so2IndexLevel = Just (Index {Index.id = Just 0, Index.indexLevelName = Just "Bardzo dobry"}),
      so2SourceDataDate = Just "2018-02-06 14:00:00", no2CalcDate = Just 1517923246000,
      Station.no2IndexLevel = Just (Index {Index.id = Just 0, Index.indexLevelName = Just "Bardzo dobry"}),
      no2SourceDataDate = Just "2018-02-06 14:00:00", coCalcDate = Just "2018-02-06 14:20:46",
      Station.coIndexLevel = Just (Index {Index.id = Just 0, Index.indexLevelName = Just "Bardzo dobry"}),
      coSourceDataDate = Just "2018-02-06 14:00:00", pm10CalcDate = Just "2018-02-06 14:20:46",
      Station.pm10IndexLevel = Just (Index {Index.id = Just 1, Index.indexLevelName = Just "Dobry"}),
      pm10SourceDataDate = Just "2018-02-06 13:00:00", pm25CalcDate = Just "2018-02-06 14:20:46",
      Station.pm25IndexLevel = Just (Index {Index.id = Just 1, Index.indexLevelName = Just "Dobry"}),
      pm25SourceDataDate = Just "2018-02-06 13:00:00", o3CalcDate = Nothing,
      Station.o3IndexLevel = Nothing, o3SourceDataDate = Nothing, c6h6CalcDate = Just "2018-02-06 14:20:46",
      Station.c6h6IndexLevel = Just (Index {Index.id = Just 0, Index.indexLevelName = Just "Bardzo dobry"}),
      c6h6SourceDataDate = Just "2018-02-06 13:00:00", stIndexStatus = Just True,
      stIndexCrParam = Just "PYL"}) Station.no2IndexLevel))



