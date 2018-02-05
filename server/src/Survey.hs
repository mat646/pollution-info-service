
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Survey
Description : Contains result of single measurement of specific compound.

Contains result of single measurement of specific compound.
-}
module Survey where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types

-- | Data for survey object.
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

