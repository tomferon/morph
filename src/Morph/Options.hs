module Morph.Options
  ( Options(..)
  , getOptions
  , withConnection
  ) where

import           Control.Exception (bracket)
import           System.Environment (getEnv)

import           Data.Monoid
import qualified Data.ByteString.Char8 as BS

import           Database.PostgreSQL.Simple

import           Options.Applicative

data Options = Options
  { optsConnectionString    :: Maybe BS.ByteString
  , optsMigrationsDirectory :: FilePath
  , optsTransaction         :: Bool
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> option (Just <$> str)
        (short 'c' <> long "connection"
          <> metavar "DATABASE_CONNECTION_STRING"
          <> help "Libpq connection string. Read from environment variable otherwise."
          <> value Nothing)
  <*> strOption
        (short 'd' <> long "dir" <> metavar "PATH"
          <> showDefault <> value "migrations"
          <> help "Path to the directory containing migrations.")
  <*> flag True False
        (long "no-transaction"
          <> help "Do not run migrations in a SQL transaction.")

getOptions :: IO Options
getOptions = execParser $ info (helper <*> optionsParser) $
  fullDesc
  <> progDesc "Migrator for PostgreSQL databases with support for rollbacks"
  <> footer "This program is licensed under the BSD-3 license."

createConn :: Options -> IO Connection
createConn opts = do
  connString <- case optsConnectionString opts of
    Nothing -> BS.pack <$> getEnv "DATABASE_CONNECTION_STRING"
    Just cs -> pure cs
  connectPostgreSQL connString

destroyConn :: Connection -> IO ()
destroyConn = close

withConnection :: Options -> (Connection -> IO a) -> IO a
withConnection options f = bracket (createConn options) destroyConn f
