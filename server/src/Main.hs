{-|
Module      : Main
Description : Module instantiating web application.

Module instantiating web application.
-}
module Main where

import           Network.Wai.Handler.Warp
import           System.IO
import           App

-- | Main method setting up environment for application
main :: IO ()
main = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr
          ("listening on port " ++ show port ++ "...")) $
        defaultSettings
  runSettings settings =<< app
