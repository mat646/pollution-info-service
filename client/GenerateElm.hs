{-# LANGUAGE OverloadedStrings #-}

import           Data.List
import           Servant.Elm
import           Data.Text hiding (intercalate, map)
import           Elm (toElmDecoderSource, toElmEncoderSource, toElmTypeSource)

import           Api
import           Table

main :: IO ()
main = do
  let code = "module Api exposing (..)" :
            defElmImports :
            "type NoContent = NoContent" :
            toElmTypeSource (Proxy :: Proxy Api) :
            toElmDecoderSource (Proxy :: Proxy Api) :
            generateElmForAPI api
  writeFile "client/Api.elm" $ intercalate "\n\n" $ map unpack code
