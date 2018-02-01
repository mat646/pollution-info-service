{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module App where

import           Control.Concurrent
import           Control.Applicative (optional)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Map
import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types
import           Network.Wai
import           Network.Wai.MakeAssets
import           Data.Maybe (fromJust)
import           Servant
import           Network.HTTP.Client (responseBody, newManager, defaultManagerSettings, parseRequest, httpLbs)

import           Api
import           Index
import           Station

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

app :: IO Application
app =
  serve withAssets <$> server

server :: IO (Server WithAssets)
server = do

-- (SENDING MOCK REQUEST AND PRINTING) start

  manager <- newManager defaultManagerSettings

  request <- parseRequest "http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/117"
  response <- httpLbs request manager
  let result = responseBody response
  --print $ result


  let parsed = decode result :: Maybe Station

  print $ result
  print $ parsed

  let xd = fmap stCalcDate parsed

  print $ xd


-- (SENDING MOCK REQUEST AND PRINTING) end

  assets <- serveAssets
  db <- mkDB
  return $ apiServer db :<|> assets

-- FromJSON data declaration


-- instance FromJSON Index where
--   parseJSON (Object v) = Index
--     <$> v .: "id"
--     <*> v .: "indexLevelName"
--
-- instance FromJSON Station where
--   parseJSON = withObject "Station" $ \v -> Station
--     <$> v .: "id"
--     <*> v .: "stCalcDate"
--     <*> v .: "stIndexLevel"
--     <*> v .: "stSourceDataDate"
--     <*> v .: "so2CalcDate"
--     <*> v .: "so2IndexLevel"
--     <*> v .: "so2SourceDataDate"
--     <*> v .: "no2CalcDate"
--     <*> v .: "no2IndexLevel"
--     <*> v .: "no2SourceDataDate"
--     <*> v .: "coCalcDate"
--     <*> optional (v .: "coIndexLevel")
--     <*> v .: "coSourceDataDate"
--     <*> v .: "pm10CalcDate"
--     <*> v .: "pm10IndexLevel"
--     <*> v .: "pm10SourceDataDate"
--     <*> v .: "pm25CalcDate"
--     <*> v .: "pm25IndexLevel"
--     <*> v .: "pm25SourceDataDate"
--     <*> v .: "o3CalcDate"
--     <*> v .: "o3IndexLevel"
--     <*> v .: "o3SourceDataDate"
--     <*> v .: "c6h6CalcDate"
--     <*> v .: "c6h6IndexLevel"
--     <*> v .: "c6h6SourceDataDate"
--     <*> v .: "stIndexStatus"
--     <*> v .: "stIndexCrParam"



---------------


apiServer :: DB -> Server Api
apiServer db =
  (liftIO $ (retStation 1)) :<|>
  (liftIO $ (retStation 2)) :<|>
  listItems db :<|>
  getItem db :<|>
  postItem db :<|>
  deleteItem db


retStation :: Int -> IO Station
retStation n = do
  manager <- newManager defaultManagerSettings
  let url n = case n of
        1 -> "http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/9153"
        2 -> "http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/9153"
        _ -> "xd"
  request <- parseRequest (url n)
  response <- httpLbs request manager
  let result = responseBody response
  let parsed = decode result :: Maybe Station
  return (fromJust parsed)




listItems :: DB -> Handler [ItemId]
listItems db = liftIO $ allItemIds db

getItem :: DB -> ItemId -> Handler Item
getItem db n = maybe (throwE err404) return =<< liftIO (lookupItem db n)

postItem :: DB -> String -> Handler ItemId
postItem db new =
  liftIO $ insertItem db new

-- fake DB

data DB = DB (MVar (Map ItemId String))

debug :: DB -> IO ()
debug (DB mvar) = readMVar mvar >>= print

mkDB :: IO DB
mkDB = DB <$> newMVar empty

insertItem :: DB -> String -> IO ItemId
insertItem (DB mvar) new = modifyMVar mvar $ \ m -> do
  let newKey = case keys m of
        [] -> 0
        ks -> succ (maximum ks)
  return (insert newKey new m, newKey)

lookupItem :: DB -> ItemId -> IO (Maybe Item)
lookupItem (DB mvar) i = do
  fmap (Item i) <$> Data.Map.lookup i <$> readMVar mvar

allItemIds :: DB -> IO [ItemId]
allItemIds (DB mvar) =
  keys <$> readMVar mvar

deleteItem :: MonadIO m => DB -> ItemId -> m NoContent
deleteItem (DB mvar) i = liftIO $ do
  modifyMVar_ mvar $ \ m -> return (delete i m)
  return NoContent
