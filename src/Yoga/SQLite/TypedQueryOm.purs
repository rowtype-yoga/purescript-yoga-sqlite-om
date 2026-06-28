module Yoga.SQLite.TypedQueryOm
  ( executeSql
  , executeSqlOne
  , executeMutation
  , executeSqlRaw
  , executeSqlRawOne
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff)
import Prim.RowList (class RowToList)
import Yoga.JSON (class ReadForeign)
import Yoga.Om as Om
import Yoga.SQLite.Schema (class ParamsToArray)
import Yoga.SQLite.Schema as Schema
import Yoga.SQLite.SQLite (Connection, QueryResult, SQL, SQLiteValue)
import Yoga.SQLite.SQLite as SQLite

executeSql
  :: forall tables result params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => Schema.Q tables result params stage
  -> { | params }
  -> Om.Om { sqlite :: Connection | r } err (Array { | result })
executeSql sqlQuery params = do
  { sqlite } <- Om.ask
  Schema.runQuery sqlite params sqlQuery # liftAff

executeSqlOne
  :: forall tables result params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => Schema.Q tables result params stage
  -> { | params }
  -> Om.Om { sqlite :: Connection | r } err (Maybe { | result })
executeSqlOne sqlQuery params = do
  { sqlite } <- Om.ask
  Schema.runQueryOne sqlite params sqlQuery # liftAff

executeMutation
  :: forall tables params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => Schema.Q tables () params stage
  -> { | params }
  -> Om.Om { sqlite :: Connection | r } err Int
executeMutation sqlQuery params = do
  { sqlite } <- Om.ask
  Schema.runExecute sqlite params sqlQuery # liftAff

executeSqlRaw :: forall r err. SQL -> Array SQLiteValue -> Om.Om { sqlite :: Connection | r } err QueryResult
executeSqlRaw sqlQuery params = do
  { sqlite } <- Om.ask
  SQLite.query sqlQuery params sqlite # liftAff

executeSqlRawOne :: forall r err. SQL -> Array SQLiteValue -> Om.Om { sqlite :: Connection | r } err (Maybe SQLite.Row)
executeSqlRawOne sqlQuery params = do
  { sqlite } <- Om.ask
  SQLite.queryOne sqlQuery params sqlite # liftAff
