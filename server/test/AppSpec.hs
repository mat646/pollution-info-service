
module AppSpec where

import           Control.Exception (throwIO, ErrorCall(..))
import           Control.Monad.Trans.Except
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp
import           Servant.API
import           Servant.Client
import           Test.Hspec

import           Api
import           App (app)
import           Table

tab1 :: ClientM Table
tab2 :: ClientM Table
tab3 :: ClientM Table
tab1 :<|> tab2 :<|> tab3 = client api

spec :: Spec
spec = do
  describe "app" $ around withApp $ do
    context "api/tab1" $ do
      it "shows station 1" $ \ host -> do
        try host tab1 `shouldReturn` 402

      context "api/tab2" $ do
        it "shows station 2" $ \ host -> do
          try host tab2 `shouldReturn` 400

      context "api/tab3" $ do
        it "shows station 3" $ \ host -> do
          try host tab3 `shouldReturn` 10120

type Host = (Manager, BaseUrl)

try :: Host -> ClientM Table -> IO Int
try (manager, baseUrl) action = do
  result <- runClientM action (ClientEnv manager baseUrl)
  case result of
    Right x -> return (Table.id x)
    Left err -> throwIO $ ErrorCall $ show err

withApp :: (Host -> IO a) -> IO a
withApp action = testWithApplication app $ \ port -> do
  manager <- newManager defaultManagerSettings
  let url = BaseUrl Http "localhost" port ""
  action (manager, url)
