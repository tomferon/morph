module Morph.Config
  ( readConfigOrDie
  , withConnection
  ) where

import           Control.Applicative
import           Control.Exception

import           Data.Aeson
import           Data.Aeson.Types hiding (Options)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text                  as T

import           System.Exit
import           System.IO

import           Database.PostgreSQL.Simple

import           Morph.Options

data Config = Config
  { configUser     :: String
  , configPassword :: String
  , configHost     :: String
  , configPort     :: Integer
  , configName     :: String
  }

instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj -> Config
    <$> obj .: "user"
    <*> obj .: "password"
    <*> obj .: "host"
    <*> obj .: "port"
    <*> obj .: "name"

parseConfig :: [T.Text] -> Value -> Parser Config
parseConfig [] = parseJSON
parseConfig (key : keys) = withObject "Parent object" $ \obj -> do
  val <- obj .: key
  parseConfig keys val

readConfigOrDie :: Options -> IO Config
readConfigOrDie opts = do
  configJSON <- BSL.readFile $ optsConfigFile opts

  let eConfig = do
        val <- eitherDecode configJSON
        parseEither (parseConfig (optsJSONPath opts)) val

  case eConfig of
    Right config -> return config
    Left err -> do
      hPutStrLn stderr $ "Can't read the config file: " ++ err
      exitFailure

createConn :: Config -> IO Connection
createConn config = connect ConnectInfo
  { connectHost     = configHost     config
  , connectUser     = configUser     config
  , connectPassword = configPassword config
  , connectDatabase = configName     config
  , connectPort     = fromInteger $ configPort config
  }

destroyConn :: Connection -> IO ()
destroyConn = close

withConnection :: Config -> (Connection -> IO a) -> IO a
withConnection config f = do
  conn <- createConn config
  res <- catch (f conn) $ \e -> destroyConn conn >> throw (e :: SomeException)
  destroyConn conn
  return res
