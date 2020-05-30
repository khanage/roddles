{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module ConfigLoader where

import           ClassyPrelude
import           Types                          ( Endpoint(..)
                                                , GlobalConfiguration(..)
                                                )
import           Control.Monad.Logger           ( MonadLogger
                                                , logDebug
                                                , logError
                                                )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadMask
                                                )
import           Control.Concurrent.Async.Refresh
                                                ( newAsyncRefreshConf
                                                , RefreshResult(..)
                                                , asyncRefreshConfSetLabel
                                                , asyncRefreshConfSetDefaultInterval
                                                , asyncRefreshConfSetCallback
                                                , newAsyncRefresh
                                                )
import           Control.Lens                   ( (^.)
                                                , (&)
                                                , to
                                                )
import qualified Network.Wreq                  as Wreq
import qualified Data.Yaml                     as Yaml
import qualified Data.ByteString.Lazy          as LBS


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

    let
        callback blah = case blah of
            Left  ex  -> $logError $ "Error loading config: " <> tshow ex
            Right res -> do
                liftIO $ atomically $ writeTVar initialConfig $ refreshResult
                    res

    let
        conf =
            newAsyncRefreshConf
                    (loadConfig endpoint <&> flip RefreshResult Nothing)
                & asyncRefreshConfSetLabel "Refresh config"
                & asyncRefreshConfSetDefaultInterval (10 * 1000) -- 10 x 1000ms
                & asyncRefreshConfSetCallback callback
    _ <- newAsyncRefresh conf
    pure initialConfig
