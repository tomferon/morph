module Morph.Options
  ( Options(..)
  , getOptions
  , withConnection
  ) where

import           Control.Exception (bracket)

import           Data.Monoid
import qualified Data.ByteString as BS
import qualified Data.Text as T

import           Database.PostgreSQL.Simple

import           Options.Applicative

data Options = Options
  { optsConnectionString    :: BS.ByteString
  , optsMigrationsDirectory :: FilePath
  , optsTransaction         :: Bool
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> option str
             (short 'c' <> long "connection" <> metavar "CONNECTION_STRING"
              <> help "Libpq connection string.")
  <*> strOption (short 'd' <> long "dir" <> metavar "PATH"
                 <> showDefault <> value "migrations"
                 <> help "Path to the directory containing migrations.")
  <*> flag True False (long "no-transaction"
                       <> help "Do not run migrations in a SQL transaction. ")

getOptions :: IO Options
getOptions = execParser $ info (helper <*> optionsParser) $
  fullDesc
  <> progDesc "Migrator for PostgreSQL databases with support for rollbacks"
  <> footer "This program is licensed under the BSD-3 license."

createConn :: Options -> IO Connection
createConn = connectPostgreSQL . optsConnectionString

destroyConn :: Connection -> IO ()
destroyConn = close

withConnection :: Options -> (Connection -> IO a) -> IO a
withConnection options f = bracket (createConn options) destroyConn f
