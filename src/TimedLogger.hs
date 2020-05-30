{-# LANGUAGE NoImplicitPrelude #-}

module TimedLogger
    ( runStdoutTimedLoggerT
    )
where

import           ClassyPrelude
import           Types                          ( LogRunner )
import qualified Data.ByteString.Char8         as S8
import qualified Data.Text                     as T
import qualified Data.Time                     as Time
import           Data.Time.LocalTime            ( getZonedTime )
import           Control.Monad.Logger

runStdoutTimedLoggerT :: LogRunner
runStdoutTimedLoggerT = (`runLoggingT` timedOutput stdout)

timedOutput :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
timedOutput h loc src level msg = do
    timeStamp <-
        getZonedTime
        <&> Time.formatTime Time.defaultTimeLocale "%H:%M:%S.%M"
        <&> T.pack
    S8.hPutStr h (ls timeStamp)
    where ls timeStamp = timedLogStrBS timeStamp loc src level msg

timedLogStrBS :: Text -> Loc -> LogSource -> LogLevel -> LogStr -> S8.ByteString
timedLogStrBS timeStamp a b c d = toBS $ timestampedLogStr timeStamp a b c d
    where toBS = fromLogStr

defaultLogLevelStr :: LogLevel -> LogStr
defaultLogLevelStr level = case level of
    LevelOther t -> toLogStr t
    _            -> toLogStr $ S8.pack $ drop 5 $ show level

timestampedLogStr :: Text -> Loc -> LogSource -> LogLevel -> LogStr -> LogStr
timestampedLogStr timestamp loc src level msg =
    "["
        <> toLogStr timestamp
        <> " "
        <> defaultLogLevelStr level
        <> (if T.null src then mempty else "#" <> toLogStr src)
        <> "] "
        <> msg
        <> (if isDefaultLoc loc
               then "\n"
               else " @(" <> toLogStr (S8.pack fileLocStr) <> ")\n"
           )
  where
    isDefaultLoc :: Loc -> Bool
    isDefaultLoc (Loc "<unknown>" "<unknown>" "<unknown>" (0, 0) (0, 0)) = True
    isDefaultLoc _ = False

    fileLocStr = (loc_filename loc) ++ ':' : (line loc) ++ ':' : (char loc)
      where
        line = show . fst . loc_start
        char = show . snd . loc_start
