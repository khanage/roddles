{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import           ClassyPrelude           hiding ( Handler )
import           Servant                        ( Accept(..)
                                                , MimeRender(..)
                                                )
import           Control.Monad.Logger           ( LoggingT )
import           Servant                        ( Handler )
import           Data.Aeson                     ( Value
                                                , ToJSON(..)
                                                )
import qualified Data.Yaml                     as Yaml
import qualified Data.ByteString.Lazy          as LBS
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )

data State = State { refreshedConfig :: TVar GlobalConfiguration, lookupEndpoint :: Endpoint }

data Arguments = Arguments {
  endpoint :: Endpoint,
  port :: Int
} deriving (Eq, Show)

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

