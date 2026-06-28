module Yoga.SQLite.OmLayer
  ( SQLiteConfig
  , SQLiteL
  , sqliteLayer
  , sqliteLayer'
  ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Console as Console
import Yoga.SQLite.SQLite (Connection)
import Yoga.SQLite.SQLite as SQLite
import Yoga.Om as Om
import Yoga.Om.Layer (OmLayer, makeLayer)

-- | SQLite configuration
type SQLiteConfig =
  { url :: String
  }

-- | Row type for SQLite service
type SQLiteL r = (sqlite :: Connection | r)

-- | Create a SQLite layer that provides DBConnection as a service
-- | Requires SQLiteConfig in context
sqliteLayer :: forall r. OmLayer (sqliteConfig :: SQLiteConfig | r) () { sqlite :: Connection }
sqliteLayer = makeLayer do
  { sqliteConfig } <- Om.ask
  logInfo "Creating SQLite connection"
  db <- liftEffect $ SQLite.sqlite sqliteConfig
  logInfo "SQLite connected"
  pure { sqlite: db }
  where
  logInfo msg = liftEffect $ Console.log msg

-- | Create a SQLite layer with inline config
-- | Useful when you don't need config from context
sqliteLayer' ::
  forall r.
  SQLiteConfig ->
  OmLayer r () { sqlite :: Connection }
sqliteLayer' config = makeLayer do
  logInfo "Creating SQLite connection"
  db <- liftEffect $ SQLite.sqlite config
  logInfo "SQLite connected"
  pure { sqlite: db }
  where
  logInfo msg = liftEffect $ Console.log msg
