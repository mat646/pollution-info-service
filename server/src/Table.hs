{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Table
Description : Object aggregating results of levels requests and
  computed average values.

Object aggregating results of levels requests and
  computed average values.
-}
module Table where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types

-- | Data containing field for every compound index level name
-- | and computed value of average pollution.
data Table = Table {
  id :: Int,
  time :: String,
  so2IndexLevel :: String,
  so2Avg :: Double,
  no2IndexLevel :: String,
  no2Avg :: Double,
  coIndexLevel :: String,
  coAvg :: Double,
  pm10IndexLevel :: String,
  pm10Avg :: Double,
  pm25IndexLevel :: String,
  pm25Avg :: Double,
  o3IndexLevel :: String,
  o3Avg :: Double,
  c6h6IndexLevel :: String,
  c6h6Avg :: Double
} deriving (Show, Generic)

instance ToJSON Table where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

instance FromJSON Table where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

