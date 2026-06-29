module Test.SQLiteOm.TypedRawSpec where

import Prelude

import Data.Array as Array
import Data.Maybe (isNothing)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (withDb)
import Yoga.SQLite.Om as SQLiteOm
import Yoga.SQLite.SQLite as SQLite
import Yoga.SQLite.TypedQueryOm as TypedQuery

spec :: Spec Unit
spec = describe "postgres-style raw SQLite query API" do
  it "executes raw SQL through the typed query module" do
    withDb do
      _ <- SQLiteOm.executeSimple (SQLite.SQL "CREATE TABLE users (email TEXT NOT NULL)")
      _ <- SQLiteOm.execute (SQLite.SQL "INSERT INTO users (email) VALUES (?1)") (SQLite.params [ "ada@example.com" ])
      result <- TypedQuery.executeSqlRaw (SQLite.SQL "SELECT email FROM users WHERE email = ?1") (SQLite.params [ "ada@example.com" ])
      Array.length result.rows `shouldEqual` 1 # liftAff
      result.columns `shouldEqual` [ "email" ] # liftAff

  it "returns Nothing for a missing raw SQL row" do
    withDb do
      _ <- SQLiteOm.executeSimple (SQLite.SQL "CREATE TABLE users (email TEXT NOT NULL)")
      row <- TypedQuery.executeSqlRawOne (SQLite.SQL "SELECT email FROM users WHERE email = ?1") (SQLite.params [ "missing@example.com" ])
      isNothing row `shouldEqual` true # liftAff
