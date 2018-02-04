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
import           Sensor
import           Survey
import           Table
import           Data.Time
import           Data.Map

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

app :: IO Application
app =
  serve withAssets <$> server

server :: IO (Server WithAssets)
server = do
  assets <- serveAssets
  tabMap <- DB <$> newMVar Data.Map.empty

  return $ apiServer tabMap :<|> assets

apiServer :: DB -> Server Api
apiServer tabMap =
  (liftIO $ (process 1 tabMap)) :<|>
  (liftIO $ (process 2 tabMap)) :<|>
  (liftIO $ (process 3 tabMap))



data DB = DB (MVar (Map Int Table))

-- handling sensors

process :: Int -> DB -> IO Table
process n (DB mvar) = modifyMVar mvar $ \ mapTab -> do
  case (Data.Map.lookup n mapTab) of
    Nothing -> do
      genTable <- getTable n
      return (insert n genTable mapTab, genTable)
    Just tab -> do
      timeDiff <- getTimeDiff (Table.time tab)
      print $ timeDiff
      if timeDiff < 4000.0
        then return (insert n tab mapTab, tab)
        else do
          genTable <- getTable n
          return (insert n genTable mapTab, genTable)


getTimeDiff :: String -> IO Double
getTimeDiff str = do
  case str of
    "null" -> return 5000.0
    _ -> do
      time <- getCurrentTime
      let local = read (str) :: LocalTime
      let abs = localTimeToUTC (minutesToTimeZone 60) local
      let diff = diffUTCTime time abs
      return (realToFrac diff)

getTable :: Int -> IO Table
getTable n = do
  station <- retStation n
  sensors <- retAvg n
  createTable (return station) sensors


retStation :: Int -> IO Station
retStation n = do
  manager <- newManager defaultManagerSettings
  let url n = case n of
        1 -> "http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/402" --9153
        2 -> "http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/400"
        _ -> "http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/10120"
  request <- parseRequest (url n)
  response <- httpLbs request manager
  let result = responseBody response
  let parsed = decode result :: Maybe Station
  return (fromJust parsed)


getSensorIds :: Int -> IO [String]
getSensorIds n =
  case n of
    1 -> return ["14707", "14730", "14734", "14727", "14731", "14729", "14733"]
    2 -> return ["400","2750","16500","2747","2752","400", "400"]
    _ -> return ["16465","16457","400","16460","16494","400", "400"]


retAvg :: Int -> IO [Double]
retAvg n = do
  let prefix = "http://api.gios.gov.pl/pjp-api/rest/data/getData/"
  ids <- getSensorIds n
  let urls = [prefix ++ (ids !! 0),prefix ++ (ids !! 1),prefix ++ (ids !! 2),prefix ++ (ids !! 3),prefix ++ (ids !! 4),prefix ++ (ids !! 5), prefix ++ (ids !! 6)]
  co <- getAvgFromUrl ((!!) urls 0)
  pm10 <- getAvgFromUrl ((!!) urls 1)
  c6h6 <- getAvgFromUrl ((!!) urls 2)
  no2 <- getAvgFromUrl ((!!) urls 3)
  pm25 <- getAvgFromUrl ((!!) urls 4)
  o3 <- getAvgFromUrl ((!!) urls 5)
  so2 <- getAvgFromUrl ((!!) urls 6)
  return [co, pm10, c6h6, no2, pm25, o3, so2]


getAvgFromUrl :: String -> IO Double
getAvgFromUrl str = do
  survey <- getLastValue str
  return (countAvg survey)

getLastValue :: String -> IO [Survey]
getLastValue s = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest s
  response <- httpLbs request manager
  let result = responseBody response
  let parsed = decode result :: Maybe Sensor
  case parsed of
    Nothing -> return []
    Just sensor -> case (values sensor) of
      Nothing -> return []
      Just tab -> return tab


countAvg :: [Survey] -> Double
countAvg tab = case tab of
  [] -> 0.0
  _ -> (/) (Prelude.foldr (\v n -> (unwrap (value v)) + n) 0 tab) (realToFrac (length tab))

unwrap :: Maybe Double -> Double
unwrap dob =
  case dob of
    Nothing -> 0
    Just x -> x

unwrapTime :: Maybe String -> String
unwrapTime time =
  case time of
    Nothing -> "null"
    Just x -> x

unwrapIO :: IO Station -> ( Station -> Maybe Index ) -> IO String
unwrapIO station fun = do
  wrappedStation <- station
  case (fun wrappedStation) of
    Nothing -> return "null"
    Just index -> return  (fromJust (Index.indexLevelName index))

createTable :: IO Station -> [Double] -> IO Table
createTable station doubl = do
  ids <- return . Station.id =<< station
  times <- return . unwrapTime . Station.stCalcDate =<< station
  so2 <- unwrapIO station Station.so2IndexLevel
  so2Double <- return (doubl !! 6)
  no2 <- unwrapIO station Station.no2IndexLevel
  no2Double <- return (doubl !! 3)
  co <- unwrapIO station Station.coIndexLevel
  coDouble <- return (doubl !! 0)
  pm10 <- unwrapIO station Station.pm10IndexLevel
  pm10Double <- return (doubl !! 1)
  pm25 <- unwrapIO station Station.pm25IndexLevel
  pm25Double <- return (doubl !! 4)
  o3 <- unwrapIO station Station.o3IndexLevel
  o3Double <- return (doubl !! 5)
  c6h6 <- unwrapIO station Station.c6h6IndexLevel
  c6h6Double <- return (doubl !! 2)
  let xd = Table { Table.id = ids, Table.time = times, Table.so2IndexLevel = so2,
               Table.so2Avg = so2Double, Table.no2IndexLevel = no2, Table.no2Avg = no2Double,
               Table.coIndexLevel = co, Table.coAvg = coDouble, Table.pm10IndexLevel = pm10,
               Table.pm10Avg = pm10Double, Table.pm25IndexLevel = pm25, Table.pm25Avg = pm25Double,
               Table.o3IndexLevel = o3, Table.o3Avg = o3Double, Table.c6h6IndexLevel = c6h6,
               Table.c6h6Avg = c6h6Double}
  return xd