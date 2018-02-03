{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Sensor where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Survey

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
