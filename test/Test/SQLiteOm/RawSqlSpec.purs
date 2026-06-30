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
      _ <- SQLiteOm.executeSimple (SQLite.SQL "CREATE TABLE users (name TEXT NOT NULL, email TEXT NOT NULL, role TEXT NOT NULL)")
      changed <- SQLiteOm.execute (SQLite.SQL "INSERT INTO users (name, email, role) VALUES (?1, ?2, ?3)") (SQLite.params [ "Ada", "ada@example.com", "admin" ])
      result <- SQLiteOm.query (SQLite.SQL "SELECT email FROM users WHERE name = ?1 AND role = ?2") (SQLite.params [ "Ada", "admin" ])
      changed `shouldEqual` 1 # liftAff
      Array.length result.rows `shouldEqual` 1 # liftAff
      result.columns `shouldEqual` [ "email" ] # liftAff

  it "runs parameterless querySimple against the same connection" do
    withDb do
      _ <- SQLiteOm.executeSimple (SQLite.SQL "CREATE TABLE users (name TEXT NOT NULL)")
      _ <- SQLiteOm.executeSimple (SQLite.SQL "INSERT INTO users (name) VALUES ('Grace')")
      result <- SQLiteOm.querySimple (SQLite.SQL "SELECT name FROM users")
      Array.length result.rows `shouldEqual` 1 # liftAff
      result.columns `shouldEqual` [ "name" ] # liftAff

  it "runs work inside a write transaction helper" do
    withDb do
      rowsInTransaction <- SQLiteOm.writeTransaction \tx -> do
        _ <- SQLite.txExecute (SQLite.SQL "CREATE TABLE users (name TEXT NOT NULL)") [] tx # liftAff
        _ <- SQLite.txExecute (SQLite.SQL "INSERT INTO users (name) VALUES (?1)") (SQLite.params [ "Ada" ]) tx # liftAff
        rows <- SQLite.txQuery (SQLite.SQL "SELECT name FROM users") [] tx # liftAff
        pure (Array.length rows.rows)
      rowsInTransaction `shouldEqual` 1 # liftAff
