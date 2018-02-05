{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Module      : App
Description : Application model with fake database.

Module contains methods for requesting @GIOS@ API and
  mechanisms for retrieving eligible data.
-}
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


-- | Returns application instance.
app :: IO Application
app =
  serve withAssets <$> server


-- | Creates instance of a server.
server :: IO (Server WithAssets)
server = do
  assets <- serveAssets
  tabMap <- DB <$> newMVar empty
  return $ apiServer tabMap :<|> assets


-- | Returns api routing for allowed requests.
apiServer :: DB -> Server Api
apiServer tabMap =
  (liftIO $ (process 1 tabMap)) :<|>
  (liftIO $ (process 2 tabMap)) :<|>
  (liftIO $ (process 3 tabMap))


-- | Scheme of fake database
data DB = DB (MVar (Map Int Table))


-- | Main method for retrieving table.
process :: Int -- ^ Index of table
  -> DB -- ^ Instance of fake db
  -> IO Table -- ^ Returns desired table
process n (DB mvar) = modifyMVar mvar $ \ tabMap -> do
  case (Data.Map.lookup n tabMap) of
    Nothing -> do
      generatedTable <- generateTable n
      return (insert n generatedTable tabMap, generatedTable)
    Just table -> do
      timeDiff <- getTimeDiff (Table.time table)
      if timeDiff < 4000.0
        then return (insert n table tabMap, table)
        else do
          generateTable <- generateTable n
          return (insert n generateTable tabMap, generateTable)


-- | Computes difference between given time (as string) and current time.
getTimeDiff :: String -> IO Double
getTimeDiff str = do
  case str of
    "null" -> return 5000.0
    _ -> do
      currentTime <- getCurrentTime
      let localTime = read (str) :: LocalTime
      let timestamp = localTimeToUTC (minutesToTimeZone 60) localTime
      let diff = diffUTCTime currentTime timestamp
      return (realToFrac diff)


-- | Generates new table if desired table does not exist or has expired.
generateTable :: Int -> IO Table
generateTable n = do
  station <- generateStation n
  sensorsAvg <- generateAvg n
  createTable (return station) sensorsAvg


-- | Returns instance of station data from @GIOS@ API.
generateStation :: Int -> IO Station
generateStation n = do
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


-- | Computes average pollution value from last 24 surveys for each compound.
generateAvg :: Int -> IO [Double]
generateAvg n = do
  let prefix = "http://api.gios.gov.pl/pjp-api/rest/data/getData/"
  ids <- getSensorIds n
  let urls = [prefix ++ (ids !! 0),prefix ++ (ids !! 1),prefix ++ (ids !! 2),prefix ++ (ids !! 3),prefix ++ (ids !! 4),prefix ++ (ids !! 5), prefix ++ (ids !! 6)]
  co <- getAvgFromUrl (urls !! 0)
  pm10 <- getAvgFromUrl (urls !! 1)
  c6h6 <- getAvgFromUrl (urls !! 2)
  no2 <- getAvgFromUrl (urls !! 3)
  pm25 <- getAvgFromUrl (urls !! 4)
  o3 <- getAvgFromUrl (urls !! 5)
  so2 <- getAvgFromUrl (urls !! 6)
  return [co, pm10, c6h6, no2, pm25, o3, so2]


-- | Returns indexes of sensors for given station.
getSensorIds :: Int -> IO [String]
getSensorIds n =
  case n of
    1 -> return ["14707", "14730", "14734", "14727", "14731", "14729", "14733"]
    2 -> return ["400","2750","16500","2747","2752","400", "400"]
    _ -> return ["16465","16457","400","16460","16494","400", "400"]


-- | Returns average pollution value for given URL.
getAvgFromUrl :: String -> IO Double
getAvgFromUrl str = do
  survey <- getValues str
  return (computeAvg survey)


-- | Handles requesting @GIOS@ API for given sensor URL.
getValues :: String -> IO [Survey]
getValues s = do
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


-- | Computes average pollution value for given array of values.
computeAvg :: [Survey] -> Double
computeAvg tab = case tab of
  [] -> 0.0
  _ -> (/) (Prelude.foldr (\v n -> (unwrapDouble (value v)) + n) 0 tab) (realToFrac (length tab))


-- | Unwraps double type.
unwrapDouble :: Maybe Double -> Double
unwrapDouble dob =
  case dob of
    Nothing -> 0
    Just x -> x


-- | Unwraps time value.
unwrapTime :: Maybe String -> String
unwrapTime time =
  case time of
    Nothing -> "null"
    Just x -> x


-- | Retrieves index value name for given field.
getLevelName :: IO Station -- ^ Wrapped station
  -> ( Station -> Maybe Index ) -- ^ Retrieving field from data Station
  -> IO String -- ^ Retrieved index value name
getLevelName station fun = do
  unwrappedStation <- station
  case (fun unwrappedStation) of
    Nothing -> return "null"
    Just index -> return (fromJust (Index.indexLevelName index))


-- | Returns new instance of Table data.
createTable :: IO Station -- ^ Instance of Station data
  -> [Double] -- ^ Array of average pollution values
  -> IO Table -- ^ Wrapped Table data instance
createTable station avgArr = do
  ids <- return . Station.id =<< station
  times <- return . unwrapTime . Station.stCalcDate =<< station
  so2 <- getLevelName station Station.so2IndexLevel
  so2Double <- return (avgArr !! 6)
  no2 <- getLevelName station Station.no2IndexLevel
  no2Double <- return (avgArr !! 3)
  co <- getLevelName station Station.coIndexLevel
  coDouble <- return (avgArr !! 0)
  pm10 <- getLevelName station Station.pm10IndexLevel
  pm10Double <- return (avgArr !! 1)
  pm25 <- getLevelName station Station.pm25IndexLevel
  pm25Double <- return (avgArr !! 4)
  o3 <- getLevelName station Station.o3IndexLevel
  o3Double <- return (avgArr !! 5)
  c6h6 <- getLevelName station Station.c6h6IndexLevel
  c6h6Double <- return (avgArr !! 2)
  return Table { Table.id = ids, Table.time = times, Table.so2IndexLevel = so2,
    Table.so2Avg = so2Double, Table.no2IndexLevel = no2, Table.no2Avg = no2Double,
    Table.coIndexLevel = co, Table.coAvg = coDouble, Table.pm10IndexLevel = pm10,
    Table.pm10Avg = pm10Double, Table.pm25IndexLevel = pm25, Table.pm25Avg = pm25Double,
    Table.o3IndexLevel = o3, Table.o3Avg = o3Double, Table.c6h6IndexLevel = c6h6,
    Table.c6h6Avg = c6h6Double}
