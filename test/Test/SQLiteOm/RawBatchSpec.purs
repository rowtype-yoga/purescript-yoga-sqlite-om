module Test.SQLiteOm.RawBatchSpec where

import Prelude

import Data.Array as Array
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (withDb)
import Yoga.SQLite.Om as SQLiteOm
import Yoga.SQLite.SQLite as SQLite

spec :: Spec Unit
spec = describe "raw SQLite batch operations" do
  it "runs a deferred batch and exposes each statement result" do
    withDb do
      results <- SQLiteOm.batch SQLite.Deferred
        [ { sql: "CREATE TABLE users (name TEXT NOT NULL)", args: [] }
        , { sql: "INSERT INTO users (name) VALUES (?1)", args: SQLite.params [ "Ada" ] }
        , { sql: "INSERT INTO users (name) VALUES (?1)", args: SQLite.params [ "Grace" ] }
        ]
      rows <- SQLiteOm.querySimple (SQLite.SQL "SELECT name FROM users ORDER BY name")
      liftAff $ Array.length results `shouldEqual` 3
      liftAff $ map _.rowsAffected results `shouldEqual` [ 0, 1, 1 ]
      liftAff $ Array.length rows.rows `shouldEqual` 2
