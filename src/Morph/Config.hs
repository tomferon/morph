module Morph.Config
  ( readConfigOrDie
  , withConnection
  ) where

import           Control.Exception

import           Data.Maybe
import qualified Data.Aeson                 as J
import qualified Data.Aeson.Types           as J hiding (Options)
import qualified Data.Bifunctor             as B
import qualified Data.Yaml                  as Y
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text                  as T

import           System.Exit
import           System.IO

import           Database.PostgreSQL.Simple

import           Morph.Options

data Config = Config
  { configUsername :: String
  , configPassword :: String
  , configHostname :: String
  , configPort     :: Integer
  , configName     :: String
  }

instance J.FromJSON Config where
  parseJSON = J.withObject "Config" $ \obj -> Config
    <$> obj J..: "username"
    <*> obj J..: "password"
    <*> obj J..: "hostname"
    <*> obj J..: "port"
    <*> obj J..: "name"

parseJSONConfig :: [T.Text] -> J.Value -> J.Parser Config
parseJSONConfig [] = J.parseJSON
parseJSONConfig (key : keys) = J.withObject "Parent object" $ \obj -> do
  val <- obj J..: key
  parseJSONConfig keys val

parseYAMLConfig :: [T.Text] -> Y.Value -> Y.Parser Config
parseYAMLConfig [] val = Y.parseJSON val
parseYAMLConfig (key : keys) (Y.Object obj) = do
  val <- obj Y..: key
  parseYAMLConfig keys val
parseYAMLConfig _ _ = fail "Object expected"

readConfigOrDie :: Options -> IO Config
readConfigOrDie opts = do
  contents <- BSL.readFile $
    fromMaybe (if optsJSONConfig opts then "config.json" else "config.yml")
              (optsConfigFile opts)

  let eConfig
        | optsJSONConfig opts = do
            val <- J.eitherDecode contents
            J.parseEither (parseJSONConfig (optsKeysPath opts)) val
        | otherwise = do
            val <- B.first show . Y.decodeEither' . BSL.toStrict $ contents
            Y.parseEither (parseYAMLConfig (optsKeysPath opts)) val

  case eConfig of
    Right config -> return config
    Left err -> do
      hPutStrLn stderr $ "Can't read the config file: " ++ err
      exitFailure

createConn :: Config -> IO Connection
createConn config = connect ConnectInfo
  { connectHost     = configHostname config
  , connectUser     = configUsername config
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
