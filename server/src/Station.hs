{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Sensor
Description : Object which aggregates set of sensors.

Object which aggregates set of sensors.
-}
module Station where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Index

-- | Data which contains key values for every recorded compound.
data Station = Station {
  id :: Int,

  stCalcDate :: Maybe String,
  stIndexLevel :: Maybe Index,
  stSourceDataDate :: Maybe String,

  so2CalcDate :: Maybe String,
  so2IndexLevel :: Maybe Index,
  so2SourceDataDate :: Maybe String,

  no2CalcDate :: Maybe Int,
  no2IndexLevel :: Maybe Index,
  no2SourceDataDate :: Maybe String,

  coCalcDate :: Maybe String,
  coIndexLevel :: Maybe Index,
  coSourceDataDate :: Maybe String,

  pm10CalcDate :: Maybe String,
  pm10IndexLevel :: Maybe Index,
  pm10SourceDataDate :: Maybe String,

  pm25CalcDate :: Maybe String,
  pm25IndexLevel :: Maybe Index,
  pm25SourceDataDate :: Maybe String,

  o3CalcDate :: Maybe String,
  o3IndexLevel :: Maybe Index,
  o3SourceDataDate :: Maybe String,

  c6h6CalcDate :: Maybe String,
  c6h6IndexLevel :: Maybe Index,
  c6h6SourceDataDate :: Maybe String,

  stIndexStatus :: Maybe Bool,
  stIndexCrParam :: Maybe String

} deriving (Show, Generic)

instance ToJSON Station where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

instance FromJSON Station where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }