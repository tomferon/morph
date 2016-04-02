module Morph.Options
  ( Options(..)
  , getOptions
  ) where

import qualified Data.Text as T

import           Options.Applicative

data Options = Options
  { optsConfigFile          :: Maybe FilePath
  , optsKeysPath            :: [T.Text]
  , optsMigrationsDirectory :: FilePath
  , optsJSONConfig          :: Bool
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> option (Just <$> str)
             (short 'c' <> long "config" <> metavar "PATH"
              <> value Nothing <> help "Path to the config file.")
  <*> option ((T.splitOn "." . T.pack) <$> str)
             (short 'p' <> long "path" <> metavar "KEY1[.KEY2[...]]"
              <> value [] <> help "The keys to traverse in the JSON to find\
                                  \ the database connection info.")
  <*> strOption (short 'd' <> long "dir" <> metavar "PATH"
                 <> showDefault <> value "migrations"
                 <> help "Path to the directory containing migrations.")
  <*> flag False True (short 'j' <> long "json"
                       <> help "Read config file as JSON.")

getOptions :: IO Options
getOptions = execParser $ info (helper <*> optionsParser) $
  fullDesc
  <> progDesc "Migrator for PostgreSQL databases with support for rollbacks"
  <> footer "This program is licensed under the BSD-3 license."
