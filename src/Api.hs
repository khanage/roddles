{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module Api
  ( startApp
  , Arguments(..)
  )
where

import           ClassyPrelude           hiding ( Handler )
import           Data.Aeson                    as Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Types                          ( YAML
                                                , State(..)
                                                , Arguments(..)
                                                , AppM
                                                , MonadHasEndpoint(..)
                                                , GlobalConfiguration(..)
                                                )
import           ConfigLoader                   ( configRefresher )
import           Control.Monad.Logger           ( logDebug
                                                , runStdoutLoggingT
                                                )

type API = "config" :> Get '[JSON, YAML] Aeson.Value

api :: Proxy API
api = Proxy

app :: State -> Application
app state = serve api (nt state server)

startApp :: Arguments -> IO ()
startApp Arguments {..} = do
  let lookupEndpoint = endpoint

  refreshedConfig <- runStdoutLoggingT $ do
    config <- configRefresher lookupEndpoint
    $logDebug $ "Running on " <> tshow port
    pure config

  run port $ app $ State { .. }

nt :: State -> AppM a -> Handler a
nt state appM = runReaderT (runStdoutLoggingT appM) state

server :: (MonadHasEndpoint m) => ServerT API m
server = globalConfig <&> getGlobalConfig
