module Yoga.SQLite.Om
  ( query
  , queryOne
  , querySimple
  , execute
  , executeSimple
  , executeMultiple
  , batch
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff)
import Yoga.Om as Om
import Yoga.SQLite.SQLite (BatchStatement, Connection, QueryResult, SQL, SQLiteValue, TransactionMode)
import Yoga.SQLite.SQLite as SQLite

query :: forall r err. SQL -> Array SQLiteValue -> Om.Om { sqlite :: Connection | r } err QueryResult
query sql params = do
  { sqlite } <- Om.ask
  SQLite.query sql params sqlite # liftAff

queryOne :: forall r err. SQL -> Array SQLiteValue -> Om.Om { sqlite :: Connection | r } err (Maybe SQLite.Row)
queryOne sql params = do
  { sqlite } <- Om.ask
  SQLite.queryOne sql params sqlite # liftAff

querySimple :: forall r err. SQL -> Om.Om { sqlite :: Connection | r } err QueryResult
querySimple sql = do
  { sqlite } <- Om.ask
  SQLite.querySimple sql sqlite # liftAff

execute :: forall r err. SQL -> Array SQLiteValue -> Om.Om { sqlite :: Connection | r } err Int
execute sql params = do
  { sqlite } <- Om.ask
  SQLite.execute sql params sqlite # liftAff

executeSimple :: forall r err. SQL -> Om.Om { sqlite :: Connection | r } err Int
executeSimple sql = do
  { sqlite } <- Om.ask
  SQLite.executeSimple sql sqlite # liftAff

executeMultiple :: forall r err. String -> Om.Om { sqlite :: Connection | r } err Unit
executeMultiple sql = do
  { sqlite } <- Om.ask
  SQLite.executeMultiple sql sqlite # liftAff

batch :: forall r err. TransactionMode -> Array BatchStatement -> Om.Om { sqlite :: Connection | r } err (Array QueryResult)
batch mode statements = do
  { sqlite } <- Om.ask
  SQLite.batch mode statements sqlite # liftAff
