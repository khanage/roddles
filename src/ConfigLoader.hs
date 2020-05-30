{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module ConfigLoader where

import           ClassyPrelude
import           Types                          ( Endpoint(..)
                                                , GlobalConfiguration(..)
                                                , DelayInMilliseconds(..)
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
import           Data.Aeson                     ( Value(Null) )


loadConfig
    :: (MonadIO m, MonadLogger m, MonadThrow m)
    => Endpoint
    -> m GlobalConfiguration
loadConfig (Endpoint blobAddress) = do
    $logDebug "Refreshing config from blob storage"
    configFile <- liftIO $ Wreq.get (unpack blobAddress)

    let downloaded = configFile ^. Wreq.responseBody . to LBS.toStrict
    decodedFile <- Yaml.decodeThrow downloaded

    pure $ GlobalConfiguration decodedFile

configRefresher
    :: (MonadLogger m, MonadMask m, MonadUnliftIO m)
    => Endpoint
    -> DelayInMilliseconds
    -> m (TVar GlobalConfiguration)
configRefresher endpoint DelayInMilliseconds {..} = do
    let defaultConfig = GlobalConfiguration Null
    configurationVariable <- newTVarIO defaultConfig

    let failedToLoad exception =
            $logError $ "Error loading config: " <> tshow exception
        successfullyLoaded =
            liftIO
                . atomically
                . writeTVar configurationVariable
                . refreshResult
        resultCallback = either failedToLoad successfullyLoaded

        loader         = loadConfig endpoint <&> flip RefreshResult Nothing
        backgroundConfigReloader =
            newAsyncRefreshConf loader
                & asyncRefreshConfSetLabel "Refresh config"
                & asyncRefreshConfSetDefaultInterval millisecondDelay
                & asyncRefreshConfSetCallback resultCallback

    newAsyncRefresh backgroundConfigReloader

    pure configurationVariable
