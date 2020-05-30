{-# LANGUAGE NoImplicitPrelude, TemplateHaskell #-}

module Api
  ( app
  , startApp
  , parseDelay
  , Arguments(..)
  , LogRunner
  )
where

import           ClassyPrelude           hiding ( Handler )
import           Data.Aeson                    as Aeson
import           Network.Wai.Handler.Warp       ( run )
import           Servant                        ( Application
                                                , Handler
                                                , Proxy(..)
                                                , ServerT
                                                , JSON
                                                , Get
                                                , (:>)
                                                , serve
                                                )
import           Types                          ( YAML
                                                , State(..)
                                                , Arguments(..)
                                                , AppM
                                                , MonadHasEndpoint(..)
                                                , GlobalConfiguration(..)
                                                , LogRunner
                                                , parseDelay
                                                )
import           ConfigLoader                   ( configRefresher )
import           Control.Monad.Logger           ( logDebug )

type API = "config" :> Get '[JSON, YAML] Aeson.Value

server :: (MonadHasEndpoint m) => ServerT API m
server = globalConfig <&> getGlobalConfig

api :: Proxy API
api = Proxy

app :: LogRunner -> State -> Application
app logger state = serve api (runMonad state logger server)

startApp :: LogRunner -> Arguments -> IO ()
startApp logRunner Arguments {..} = do
  let lookupEndpoint = endpoint
  let startupMessage = "Running on port " <> tshow port

  refreshedConfig <- logRunner $ do
    config <- configRefresher lookupEndpoint refreshDelay
    $logDebug startupMessage
    pure config

  run port $ app logRunner State { .. }

runMonad :: State -> LogRunner -> AppM a -> Handler a
runMonad state logger appM = runReaderT (logger appM) state
