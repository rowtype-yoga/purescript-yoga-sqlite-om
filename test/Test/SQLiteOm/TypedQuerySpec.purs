module Test.SQLiteOm.TypedQuerySpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (createUsersTable, insertUser, usersTable, withDb)
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.ClientOm (from, insert, where_)
import Yoga.SQLite.TypedQueryOm as TypedQuery

spec :: Spec Unit
spec = describe "postgres-style SQLite typed query API" do
  it "executes typed SELECT queries with typed results" do
    withDb do
      createUsersTable
      _ <- insertUser "Ada" "ada@example.com" "admin"
      rows <- TypedQuery.executeSql (from usersTable # Client.select @"name, email, role" # where_ @"email = $email") { email: "ada@example.com" }
      rows `shouldEqual` [ { name: "Ada", email: "ada@example.com", role: "admin" } ] # liftAff

  it "returns Nothing for a missing typed row" do
    withDb do
      createUsersTable
      row <- TypedQuery.executeSqlOne (from usersTable # Client.select @"name, email, role" # where_ @"email = $email") { email: "missing@example.com" }
      row `shouldEqual` (Nothing :: Maybe { name :: String, email :: String, role :: String }) # liftAff

  it "executes typed mutations and makes them visible to typed query reads" do
    withDb do
      createUsersTable
      changed <- TypedQuery.executeMutation (from usersTable # insert { name: "Grace", email: "grace@example.com", role: "member" }) {}
      row <- TypedQuery.executeSqlOne (from usersTable # Client.select @"name, email, role" # where_ @"email = $email") { email: "grace@example.com" }
      changed `shouldEqual` 1 # liftAff
      row `shouldEqual` Just { name: "Grace", email: "grace@example.com", role: "member" } # liftAff
