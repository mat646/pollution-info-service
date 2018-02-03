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

type WithAssets = Api :<|> Raw

withAssets :: Proxy WithAssets
withAssets = Proxy

app :: IO Application
app =
  serve withAssets <$> server

server :: IO (Server WithAssets)
server = do
  assets <- serveAssets
  return $ apiServer :<|> assets

apiServer :: Server Api
apiServer =
  (liftIO $ (getTable 1)) :<|>
  (liftIO $ (getTable 2))


-- handling sensors

getTable :: Int -> IO Table
getTable n = do
  let station = retStation n
  sensors <- retAvg n
  createTable station sensors


retStation :: Int -> IO Station
retStation n = do
  manager <- newManager defaultManagerSettings
  let url n = case n of
        1 -> "http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/9153"
        2 -> "http://api.gios.gov.pl/pjp-api/rest/aqindex/getIndex/9153" --117
        _ -> "xd"
  request <- parseRequest (url n)
  response <- httpLbs request manager
  let result = responseBody response
  let parsed = decode result :: Maybe Station
  return (fromJust parsed)


getSensorIds :: Int -> IO [String]
getSensorIds n =
  case n of
    1 -> return ["2783", "2792", "2779", "2788", "2794", "2797"]
    2 -> return ["2745","2750","16500","2747","2747","2747"]
    3 -> return []
    _ -> return []


retAvg :: Int -> IO [Double]
retAvg n = do
  let prefix = "http://api.gios.gov.pl/pjp-api/rest/data/getData/"
  ids <- getSensorIds n
  let urls = [prefix ++ (ids !! 0),prefix ++ (ids !! 1),prefix ++ (ids !! 2),prefix ++ (ids !! 3),prefix ++ (ids !! 4),prefix ++ (ids !! 5)]
  co <- getAvgFromUrl ((!!) urls 0)
  pm10 <- getAvgFromUrl ((!!) urls 1)
  c6h6 <- getAvgFromUrl ((!!) urls 2)
  no2 <- getAvgFromUrl ((!!) urls 3)
  pm25 <- getAvgFromUrl ((!!) urls 4)
  so2 <- getAvgFromUrl ((!!) urls 5)
  return [co, pm10, c6h6, no2, pm25, so2]


getAvgFromUrl :: String -> IO Double
getAvgFromUrl str = return .countAvg =<< (getLastValue str)

getLastValue :: String -> IO [Survey]
getLastValue s = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest s
  response <- httpLbs request manager
  let result = responseBody response
  let parsed = decode result :: Maybe Sensor
  print $ (fromJust parsed)
  return (values (fromJust parsed))


countAvg :: [Survey] -> Double
countAvg tab = (/) (Prelude.foldr (\v n -> (unwrap (value v)) + n) 0 tab) (realToFrac (length tab))

unwrap :: Maybe Double -> Double
unwrap dob =
  case dob of
    Nothing -> 0
    Just x -> x

unwrapIO :: IO Station -> ( Station -> Maybe Index ) -> IO String
unwrapIO station xd = return . fromJust . Index.indexLevelName =<< return . fromJust . xd =<< station

createTable :: IO Station -> [Double] -> IO Table
createTable station doubl = do
  ids <- return . Station.id =<< station
  times <- return . fromJust . Station.stCalcDate =<< station
  so2 <- unwrapIO station Station.so2IndexLevel
  so2Double <- return (doubl !! 0)
  no2 <- unwrapIO station Station.no2IndexLevel
  no2Double <- return (doubl !! 1)
  co <- unwrapIO station Station.coIndexLevel
  coDouble <- return (doubl !! 2)
  pm10 <- unwrapIO station Station.pm10IndexLevel
  pm10Double <- return (doubl !! 3)
  pm25 <- unwrapIO station Station.pm25IndexLevel
  pm25Double <- return (doubl !! 4)
  o3 <- unwrapIO station Station.o3IndexLevel
  o3Double <- return (doubl !! 5)
  c6h6 <- unwrapIO station Station.c6h6IndexLevel
  c6h6Double <- return (doubl !! 5)
  return Table { Table.id = ids, Table.time = times, Table.so2IndexLevel = so2,
    Table.so2Avg = so2Double, Table.no2IndexLevel = no2, Table.no2Avg = no2Double,
    Table.coIndexLevel = co, Table.coAvg = coDouble, Table.pm10IndexLevel = pm10,
    Table.pm10Avg = pm10Double, Table.pm25IndexLevel = pm25, Table.pm25Avg = pm25Double,
    Table.o3IndexLevel = o3, Table.o3Avg = o3Double, Table.c6h6IndexLevel = c6h6,
    Table.c6h6Avg = c6h6Double}