{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : Index
Description : Module for compound pollution index level.

Module for compound pollution index level.
-}
module Index where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types

-- | Object representing single compound with its index level.
data Index = Index {
  id :: Maybe Int,
  indexLevelName :: Maybe String
} deriving (Show, Generic)

instance ToJSON Index where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

instance FromJSON Index where
  parseJSON = genericParseJSON defaultOptions
    { omitNothingFields = True }