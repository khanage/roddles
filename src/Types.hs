{-# LANGUAGE NoImplicitPrelude #-}

module Types where

import           ClassyPrelude           hiding ( Handler
                                                , pack
                                                , unpack
                                                )
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
import           Options.Applicative            ( eitherReader
                                                , ReadM
                                                )
import qualified Data.Attoparsec.Text          as A
import qualified Data.Text                     as T
import           Data.Char                      ( digitToInt )
import qualified Control.Newtype               as Newtype

data State = State { refreshedConfig :: TVar GlobalConfiguration, lookupEndpoint :: Endpoint }

data Arguments = Arguments {
  endpoint :: Endpoint,
  port :: Int,
  refreshDelay :: DelayInMilliseconds
} deriving (Eq, Show)

type AppM = LoggingT (ReaderT State Handler)

newtype Endpoint = Endpoint Text
  deriving (Eq, IsString, Show)

instance Newtype.Newtype Endpoint Text where
    pack = Endpoint
    unpack (Endpoint t) = t

newtype GlobalConfiguration = GlobalConfiguration { getGlobalConfig :: Value }
  deriving (Eq, Show)

class Monad m => MonadHasEndpoint m where
  globalConfig :: m GlobalConfiguration

instance MonadHasEndpoint AppM where
    globalConfig = ask <&> refreshedConfig >>= liftIO . readTVarIO

data YAML

instance Accept YAML where
    contentType _ = "text" // "yaml" /: ("charset", "utf-8")

instance ToJSON a => MimeRender YAML a where
    mimeRender _ = LBS.fromStrict . Yaml.encode

type LogRunner = forall m a . LoggingT m a -> m a

newtype DelayInMilliseconds = DelayInMilliseconds { millisecondDelay :: Int} deriving (Eq, Show, Num)

instance Newtype.Newtype DelayInMilliseconds Int where
    pack   = DelayInMilliseconds
    unpack = millisecondDelay

parseDelay :: ReadM DelayInMilliseconds
parseDelay =
    DelayInMilliseconds <$> eitherReader (A.parseOnly stringParser . T.pack)

stringParser :: A.Parser Int
stringParser = A.try parseFromString <|> parseActualNumber
  where
    parseActualNumber = round <$> A.scientific <* A.endOfInput

    parseFromString =
        joiner <$> pair <*> withLeadingColon pair <*> withLeadingColon pair
    pair = pairing <$> digitParser <*> digitParser

    withLeadingColon p = A.char ':' *> p
    digitParser = A.digit <&> digitToInt

    pairing ten unit = (ten * 10) + unit
    joiner hours minutes seconds =
        (*) 1000 $ (hours * 60 * 60) + (minutes * 60) + seconds
