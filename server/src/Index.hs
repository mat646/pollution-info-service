{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Index where

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types

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