module Test.SQLiteOm.RawSqlSpec where

import Prelude

import Data.Array as Array
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (withDb)
import Yoga.SQLite.Om as SQLiteOm
import Yoga.SQLite.SQLite as SQLite

spec :: Spec Unit
spec = describe "raw SQLite Om operations" do
  it "executes statements with parameters and returns query metadata" do
    withDb do
      _ <- SQLiteOm.executeSimple (SQLite.SQL "CREATE TABLE facts (module TEXT NOT NULL, ident TEXT NOT NULL, ps_type TEXT NOT NULL)")
      changed <- SQLiteOm.execute (SQLite.SQL "INSERT INTO facts (module, ident, ps_type) VALUES (?1, ?2, ?3)") (SQLite.params [ "Data.Foldable", "elem", "forall a. a" ])
      result <- SQLiteOm.query (SQLite.SQL "SELECT ps_type FROM facts WHERE module = ?1 AND ident = ?2") (SQLite.params [ "Data.Foldable", "elem" ])
      liftAff $ changed `shouldEqual` 1
      liftAff $ Array.length result.rows `shouldEqual` 1
      liftAff $ result.columns `shouldEqual` [ "ps_type" ]

  it "runs parameterless querySimple against the same connection" do
    withDb do
      _ <- SQLiteOm.executeSimple (SQLite.SQL "CREATE TABLE facts (ident TEXT NOT NULL)")
      _ <- SQLiteOm.executeSimple (SQLite.SQL "INSERT INTO facts (ident) VALUES ('minimum')")
      result <- SQLiteOm.querySimple (SQLite.SQL "SELECT ident FROM facts")
      liftAff $ Array.length result.rows `shouldEqual` 1
      liftAff $ result.columns `shouldEqual` [ "ident" ]
