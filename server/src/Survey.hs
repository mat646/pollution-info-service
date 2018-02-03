
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Survey where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types

data Survey = Survey {
  date :: String,
  value :: Maybe Double
} deriving (Show, Generic)

instance ToJSON Survey where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

instance FromJSON Survey where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }

