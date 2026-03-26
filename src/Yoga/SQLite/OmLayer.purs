module Yoga.SQLite.OmLayer
  ( SQLiteConfig
  , SQLiteL
  , sqliteLayer
  , sqliteLayer'
  ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console as Console
import Yoga.BunSQLite.BunSQLite (Database, DatabasePath(..))
import Yoga.BunSQLite.BunSQLite as BunSQLite
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer)

-- | SQLite configuration
type SQLiteConfig =
  { path :: DatabasePath
  }

-- | Row type for SQLite service
type SQLiteL r = (sqlite :: Database | r)

-- | Create a SQLite layer that provides Database as a service
-- | Requires SQLiteConfig in context
sqliteLayer :: forall r. OmLayer (sqliteConfig :: SQLiteConfig | r) () { sqlite :: Database }
sqliteLayer = makeLayer do
  { sqliteConfig } <- Om.ask
  logInfo "Creating SQLite connection"
  db <- liftEffect $ BunSQLite.open sqliteConfig.path
  logInfo "SQLite connected"
  pure { sqlite: db }
  where
  logInfo msg = liftEffect $ Console.log msg

-- | Create a SQLite layer with inline config
-- | Useful when you don't need config from context
sqliteLayer' ::
  forall r.
  SQLiteConfig ->
  OmLayer r () { sqlite :: Database }
sqliteLayer' config = makeLayer do
  logInfo "Creating SQLite connection"
  db <- liftEffect $ BunSQLite.open config.path
  logInfo "SQLite connected"
  pure { sqlite: db }
  where
  logInfo msg = liftEffect $ Console.log msg
