{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Station where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Index

data Station = Station {
  id :: Int,

  stCalcDate :: String,
  stIndexLevel :: Index,
  stSourceDataDate :: String,

  so2CalcDate :: String,
  so2IndexLevel :: Index,
  so2SourceDataDate :: String,

  no2CalcDate :: Int,
  no2IndexLevel :: Index,
  no2SourceDataDate :: String,

  coCalcDate :: String,
  coIndexLevel :: Index,
  coSourceDataDate :: String,

  pm10CalcDate :: String,
  pm10IndexLevel :: Index,
  pm10SourceDataDate :: String,

  pm25CalcDate :: String,
  pm25IndexLevel :: Index,
  pm25SourceDataDate :: String,

  o3CalcDate :: String,
  o3IndexLevel :: Index,
  o3SourceDataDate :: String,

  c6h6CalcDate :: String,
  c6h6IndexLevel :: Index,
  c6h6SourceDataDate :: String,

  stIndexStatus :: Bool,
  stIndexCrParam :: String

} deriving (Show, Generic)

instance ToJSON Station where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

instance FromJSON Station where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }