module Test.SQLiteOm.Support
  ( FactsTable
  , factsTable
  , withDb
  , createFactsTable
  , insertFact
  ) where

import Prelude

import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Yoga.Om as Om
import Yoga.SQLite.ClientOm (AutoIncrement, PrimaryKey, Table, createTableDDL, from, insert)
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.Om as SQLiteOm
import Yoga.SQLite.SQLite as SQLite

type FactsTable = Table "facts"
  ( id :: Int # PrimaryKey # AutoIncrement
  , module :: String
  , ident :: String
  , ps_type :: String
  )

factsTable :: Proxy FactsTable
factsTable = Proxy

withDb :: Om.Om { sqlite :: SQLite.Connection } () Unit -> Aff Unit
withDb om = do
  conn <- liftEffect $ SQLite.sqlite { url: ":memory:" }
  _ <- Om.runOm { sqlite: conn } { exception: throwError } om
  SQLite.close conn # liftEffect

createFactsTable :: forall err. Om.Om { sqlite :: SQLite.Connection } err Unit
createFactsTable = do
  SQLiteOm.executeSimple (SQLite.SQL (createTableDDL @FactsTable)) # void

insertFact :: forall err. String -> String -> String -> Om.Om { sqlite :: SQLite.Connection } err Int
insertFact moduleName ident psType =
  Client.execCount {} (from factsTable # insert { module: moduleName, ident, ps_type: psType })
