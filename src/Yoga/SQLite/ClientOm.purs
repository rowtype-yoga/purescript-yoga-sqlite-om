module Yoga.SQLite.ClientOm
  ( run
  , runOne
  , exec
  , execCount
  , module Schema
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff)
import Prim.RowList (class RowToList)
import Yoga.JSON (class ReadForeign)
import Yoga.Om as Om
import Yoga.SQLite.Schema as Schema
import Yoga.SQLite.Schema (class ParamsToArray)
import Yoga.SQLite.SQLite (Connection)

run
  :: forall tables result params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => { | params }
  -> Schema.Q tables result params stage
  -> Om.Om { sqlite :: Connection | r } err (Array { | result })
run params q = do
  { sqlite } <- Om.ask
  Schema.runQuery sqlite params q # liftAff

runOne
  :: forall tables result params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => { | params }
  -> Schema.Q tables result params stage
  -> Om.Om { sqlite :: Connection | r } err (Maybe { | result })
runOne params q = do
  { sqlite } <- Om.ask
  Schema.runQueryOne sqlite params q # liftAff

exec
  :: forall tables params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => { | params }
  -> Schema.Q tables () params stage
  -> Om.Om { sqlite :: Connection | r } err Unit
exec params q = do
  { sqlite } <- Om.ask
  Schema.runExecute sqlite params q # liftAff # void

execCount
  :: forall tables params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => { | params }
  -> Schema.Q tables () params stage
  -> Om.Om { sqlite :: Connection | r } err Int
execCount params q = do
  { sqlite } <- Om.ask
  Schema.runExecute sqlite params q # liftAff
