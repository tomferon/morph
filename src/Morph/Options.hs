module Morph.Options
  ( Options(..)
  , getOptions
  ) where

import qualified Data.Text as T

import           Options.Applicative

data Options = Options
  { optsConfigFile          :: FilePath
  , optsJSONPath            :: [T.Text]
  , optsMigrationsDirectory :: FilePath
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (short 'c' <> long "config" <> metavar "PATH"
                 <> showDefault <> value "config.json"
                 <> help "Path to the config file.")
  <*> option ((T.splitOn "." . T.pack) <$> str)
             (short 'p' <> long "path" <> metavar "KEY1[.KEY2[...]]"
              <> value [] <> help "The keys to traverse in the JSON to find\
                                  \ the database connection info.")
  <*> strOption (short 'd' <> long "dir" <> metavar "PATH"
                 <> showDefault <> value "migrations"
                 <> help "Path to the directory containing migrations.")

getOptions :: IO Options
getOptions = execParser $ info (helper <*> optionsParser) $
  fullDesc
  <> progDesc "Migrator for PostgreSQL databases with support for rollbacks"
  <> footer "This program is licensed under the BSD-3 license."
