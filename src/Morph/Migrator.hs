{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Morph.Migrator
  ( migrate
  ) where

import Control.Monad

import Data.List
import Data.Monoid
import Data.String

import System.FilePath
import System.Directory

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

-- | A migration can either be read from file and contain both sides or from the
-- database and contain only the down side.
data MigrationType = Full | Rollback

type family MigrationSQL (a :: MigrationType) :: * where
  MigrationSQL 'Full     = (Query, String)
  MigrationSQL 'Rollback = Query

data Migration :: MigrationType -> * where
  Migration ::
    { migrationIdentifier :: String
    , migrationSQL        :: MigrationSQL a
    } -> Migration a

instance FromRow (Migration 'Rollback) where
  fromRow = Migration <$> field <*> fmap fromString field

createMigrationTable :: Connection -> IO ()
createMigrationTable conn = void $ execute_ conn
  "CREATE TABLE IF NOT EXISTS migrations (\
  \  id           varchar PRIMARY KEY CHECK (id <> ''),\
  \  rollback_sql text CHECK (rollback_sql <> '')\
  \);"

listDone :: Connection -> IO [Migration 'Rollback]
listDone = flip query_ "SELECT id, rollback_sql FROM migrations ORDER BY id ASC"

listGoals :: FilePath -> IO [Migration 'Full]
listGoals dir = do
    allNames <- sort <$> getDirectoryContents dir
    let upNames     = filter (".up.sql"   `isSuffixOf`) allNames
        downNames   = filter (".down.sql" `isSuffixOf`) allNames

    forM upNames $ \upName -> do
      let identifier = extractIdentifier upName
      up   <- readMigrationFile upName
      down <- readDownMigrationFile downNames identifier
      return Migration
        { migrationIdentifier = identifier
        , migrationSQL        = (up, down)
        }

  where
    extractIdentifier :: FilePath -> String
    extractIdentifier = takeWhile (`elem` ("0123456789" :: String))

    readMigrationFile :: FilePath -> IO Query
    readMigrationFile path = do
      contents <- readFile $ dir </> path
      return $ fromString contents

    readDownMigrationFile :: [FilePath] -> String -> IO String
    readDownMigrationFile paths identifier =
      case find ((==identifier) . extractIdentifier) paths of
        Nothing -> return $
          "RAISE EXCEPTION 'No rollback migration found for "
          <> fromString identifier <> "';"
        Just path -> readFile $ dir </> path

rollbackMigration :: Connection -> Migration 'Rollback -> IO ()
rollbackMigration conn migration = do
  void $ execute_ conn $ migrationSQL migration
  void $ execute conn "DELETE FROM migrations WHERE id = ?" $
    Only $ migrationIdentifier migration

doMigration :: Connection -> Migration 'Full -> IO ()
doMigration conn migration = do
  let (up, down) = migrationSQL migration
  void $ execute_ conn up
  void $ execute conn "INSERT INTO migrations (id, rollback_sql) VALUES (?, ?)"
                 (migrationIdentifier migration, down)

migrate :: Connection -> FilePath -> IO ()
migrate conn dir = do
  createMigrationTable conn

  doneMigrations <- listDone  conn
  goalMigrations <- listGoals dir

  let doneIdentifiers = map migrationIdentifier doneMigrations
      goalIdentifiers = map migrationIdentifier goalMigrations

      toRollbackIdentifiers = doneIdentifiers \\ goalIdentifiers
      toDoIdentifiers       = goalIdentifiers \\ doneIdentifiers

      toRollback = filter ((`elem` toRollbackIdentifiers) . migrationIdentifier)
                          doneMigrations
      toDo       = filter ((`elem` toDoIdentifiers)       . migrationIdentifier)
                          goalMigrations

  withTransaction conn $ do
    forM_ toRollback $ rollbackMigration conn
    forM_ toDo       $ doMigration       conn
