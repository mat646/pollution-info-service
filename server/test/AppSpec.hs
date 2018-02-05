
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

getTab :: ClientM Table
getTab = client api

spec :: Spec
spec = do
  describe "app" $ around withApp $ do
    context "api/tab1" $ do
      it "shows station 1" $ \ host -> do
        try host getTab `shouldReturn` Table -- not working

type Host = (Manager, BaseUrl)

try :: Host -> ClientM a -> IO a
try (manager, baseUrl) action = do
  result <- runClientM action (ClientEnv manager baseUrl)
  case result of
    Right x -> return x
    Left err -> throwIO $ ErrorCall $ show err

withApp :: (Host -> IO a) -> IO a
withApp action = testWithApplication app $ \ port -> do
  manager <- newManager defaultManagerSettings
  let url = BaseUrl Http "localhost" port ""
  action (manager, url)
