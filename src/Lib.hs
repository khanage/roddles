{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( startApp
  , app
  , Arguments(..)
  )
where

import           ClassyPrelude           hiding ( Handler )

import           Control.Lens                   ( to
                                                , (^.)
                                                , (&)
                                                )
import qualified Data.ByteString.Lazy          as LBS
import qualified Network.Wreq                  as Wreq
import           Data.Aeson                    as Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Data.Yaml                     as Yaml
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadMask
                                                )
import           Control.Monad.Logger           ( MonadLogger
                                                , LoggingT
                                                , runStdoutLoggingT
                                                , logDebug
                                                , logError
                                                )
import           Control.Concurrent.Async.Refresh
                                                ( newAsyncRefreshConf
                                                , RefreshResult(..)
                                                , asyncRefreshConfSetLabel
                                                , asyncRefreshConfSetDefaultInterval
                                                , asyncRefreshConfSetCallback
                                                , newAsyncRefresh
                                                )

type API = "config" :> Get '[JSON, YAML] Aeson.Value

app :: State -> Application
app state = serve api (nt state server)

api :: Proxy API
api = Proxy

data State = State { refreshedConfig :: TVar GlobalConfiguration, lookupEndpoint :: Endpoint }

data Arguments = Arguments {
  endpoint :: Endpoint,
  port :: Int
} deriving (Eq, Show)

startApp :: Arguments -> IO ()
startApp Arguments {..} = do
  let lookupEndpoint = endpoint

  refreshedConfig <- runStdoutLoggingT $ do
    config <- configRefresher lookupEndpoint
    $logDebug $ "Running on " <> tshow port
    pure config

  run port $ app (State { lookupEndpoint, refreshedConfig })

nt :: State -> AppM a -> Handler a
nt state appM = runReaderT (runStdoutLoggingT appM) state

loadConfig
  :: (MonadIO m, MonadLogger m, MonadThrow m)
  => Endpoint
  -> m GlobalConfiguration
loadConfig (Endpoint blobAddress) = do
  $(logDebug) "Refreshing config from blob storage"
  configFile <- liftIO $ Wreq.get (unpack blobAddress)

  let downloaded = configFile ^. Wreq.responseBody . to LBS.toStrict
  decodedFile <- Yaml.decodeThrow downloaded

  pure $ GlobalConfiguration decodedFile

configRefresher
  :: (MonadLogger m, MonadMask m, MonadUnliftIO m)
  => Endpoint
  -> m (TVar GlobalConfiguration)
configRefresher endpoint = do
  loadedConfig  <- loadConfig endpoint
  initialConfig <- newTVarIO loadedConfig

  let callback blah = case blah of
        Left  ex  -> $logError $ "Error loading config: " <> tshow ex
        Right res -> do
          liftIO $ atomically $ writeTVar initialConfig $ refreshResult res

  let conf =
        newAsyncRefreshConf (loadConfig endpoint <&> flip RefreshResult Nothing)
          & asyncRefreshConfSetLabel "Refresh config"
          & asyncRefreshConfSetDefaultInterval (10 * 1000) -- 10 x 1000ms
          & asyncRefreshConfSetCallback callback
  _ <- newAsyncRefresh conf
  pure initialConfig

server :: (MonadHasEndpoint m) => ServerT API m
server = globalConfig <&> getGlobalConfig

type AppM = LoggingT (ReaderT State Handler)

newtype Endpoint = Endpoint Text
  deriving (Eq, IsString, Show)

newtype GlobalConfiguration = GlobalConfiguration { getGlobalConfig :: Value }
  deriving (Eq, Show)

class Monad m => MonadHasEndpoint m where
  globalConfig :: m GlobalConfiguration

instance MonadHasEndpoint AppM where
  globalConfig = do
    tvarConfig <- refreshedConfig <$> ask
    liftIO $ readTVarIO tvarConfig

data YAML

instance Accept YAML where
  contentType _ = "text" // "yaml" /: ("charset", "utf-8")

instance ToJSON a => MimeRender YAML a where
  mimeRender _ = LBS.fromStrict . Yaml.encode
