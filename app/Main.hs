{-# LANGUAGE TemplateHaskell, NumericUnderscores, OverloadedStrings, NoImplicitPrelude, RecordWildCards #-}
module Main where

import           ClassyPrelude           hiding ( unpack )
import           Control.Monad.Logger
import           Options.Applicative
import           Api                            ( LogRunner
                                                , startApp
                                                , parseDelay
                                                , Arguments(..)
                                                )
import           TimedLogger                    ( runStdoutTimedLoggerT )
import           Control.Newtype                ( unpack )

main :: IO ()
main = do
  args <- execParser opts
  logger $ $logInfo (startupMessage args)
  startApp logger args
 where
  opts = info
    (argParser <**> helper)
    (  fullDesc
    <> progDesc "Provide either JSON or YAML for a YAML or JSON file"
    <> header "roddles - I did it for you mate."
    )

  logger :: LogRunner
  logger = runStdoutTimedLoggerT

  startupMessage Arguments {..} =
    "Loading application to refresh "
      <> tshow (unpack endpoint)
      <> " every "
      <> tshow (unpack refreshDelay)
      <> "ms."

argParser :: Parser Arguments
argParser =
  Arguments
    <$> strOption
          (short 'c' <> long "config-uri" <> metavar "CONFIG_URI" <> help
            "The blob storage uri to hit"
          )
    <*> option
          auto
          (  short 'p'
          <> long "port"
          <> metavar "PORT"
          <> help "Port to run on"
          <> value 8080
          <> showDefault
          )
    <*> option
          parseDelay
          (  short 'd'
          <> long "delay"
          <> metavar "DELAY_BETWEEN_REFRESH_MS"
          <> value 10_000
          <> help
               "Either provide a number in milliseconds, or use '00:00:00' as hours, minutes, seconds."
          )
