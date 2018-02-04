{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Servant.API
import           Servant.Elm      (ElmType)
import           Station
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Table

type Api =
  "api" :>
    ("tab1" :> Get '[JSON] Table :<|>
     "tab2" :> Get '[JSON] Table :<|>
     "tab3" :> Get '[JSON] Table)

api :: Proxy Api
api = Proxy
