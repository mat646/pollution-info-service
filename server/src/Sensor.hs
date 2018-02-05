{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Sensor
Description : Object containing set of survey values.

Object containing set of survey values.
-}
module Sensor where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Survey

-- | Data for sensor with set of measurement arrays
data Sensor = Sensor {
  key :: String,
  values :: Maybe [Survey]
} deriving (Show, Generic)

instance ToJSON Sensor where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

instance FromJSON Sensor where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }
