{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad.Logger           ( runStdoutLoggingT
                                                , logDebugSH
                                                )
import           Options.Applicative
import           Api                            ( startApp
                                                , Arguments(..)
                                                )

main :: IO ()
main = do
  args <- execParser opts
  runStdoutLoggingT $ $logDebugSH args
  startApp args
 where
  opts = info
    (argParser <**> helper)
    (  fullDesc
    <> progDesc "Provide either JSON or YAML for a YAML or JSON file"
    <> header "roddles - I did it for you mate."
    )

argParser :: Parser Arguments
argParser =
  Arguments
    <$> strOption
          (short 'c' <> long "config-uri" <> metavar "config" <> help
            "The blob storage uri to hit"
          )
    <*> option
          auto
          (  short 'p'
          <> long "port"
          <> metavar "port"
          <> help "Port to run on"
          <> value 8080
          <> showDefault
          )
