module Test.SQLiteOm.TypedQuerySpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (createFactsTable, factsTable, insertFact, withDb)
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.ClientOm (from, insert, where_)
import Yoga.SQLite.TypedQueryOm as TypedQuery

spec :: Spec Unit
spec = describe "postgres-style SQLite typed query API" do
  it "executes typed SELECT queries with typed results" do
    withDb do
      createFactsTable
      _ <- insertFact "Data.Semigroup.Foldable" "minimum" "forall f a. Foldable1 f => Ord a => f a -> a"
      rows <- TypedQuery.executeSql (from factsTable # Client.select @"module, ident, ps_type" # where_ @"ident = $ident") { ident: "minimum" }
      liftAff $ rows `shouldEqual` [ { module: "Data.Semigroup.Foldable", ident: "minimum", ps_type: "forall f a. Foldable1 f => Ord a => f a -> a" } ]

  it "returns Nothing for a missing typed row" do
    withDb do
      createFactsTable
      row <- TypedQuery.executeSqlOne (from factsTable # Client.select @"module, ident, ps_type" # where_ @"ident = $ident") { ident: "missing" }
      liftAff $ row `shouldEqual` (Nothing :: Maybe { module :: String, ident :: String, ps_type :: String })

  it "executes typed mutations and makes them visible to typed query reads" do
    withDb do
      createFactsTable
      changed <- TypedQuery.executeMutation (from factsTable # insert { module: "Data.Semigroup.Foldable", ident: "maximum", ps_type: "forall f a. Foldable1 f => Ord a => f a -> a" }) {}
      row <- TypedQuery.executeSqlOne (from factsTable # Client.select @"module, ident, ps_type" # where_ @"ident = $ident") { ident: "maximum" }
      liftAff $ changed `shouldEqual` 1
      liftAff $ row `shouldEqual` Just { module: "Data.Semigroup.Foldable", ident: "maximum", ps_type: "forall f a. Foldable1 f => Ord a => f a -> a" }
