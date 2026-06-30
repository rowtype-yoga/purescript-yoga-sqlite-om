module Test.SQLiteOm.ClientReadSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (createUsersTable, usersTable, withDb, withDbParsed)
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.ClientOm (from, insert, returning, where_)

spec :: Spec Unit
spec = describe "typed ClientOm reads" do
  it "inserts a typed row and returns a typed projection" do
    withDb do
      createUsersTable
      created <- Client.runOne {} (from usersTable # insert { name: "Ada", email: "ada@example.com", role: "admin" } # returning @"name, email, role")
      created `shouldEqual` Just { name: "Ada", email: "ada@example.com", role: "admin" } # liftAff

  it "selects typed rows with named parameters" do
    withDb do
      createUsersTable
      let exec = Client.execCount {}
      _ <- from usersTable # insert { name: "Ada", email: "ada@example.com", role: "admin" } # exec
      _ <- from usersTable # insert { name: "Grace", email: "grace@example.com", role: "member" } # exec
      rows <- Client.run { roleName: "admin" } (from usersTable # Client.select @"name, email, role" # where_ @"role = $roleName")
      rows `shouldEqual` [ { name: "Ada", email: "ada@example.com", role: "admin" } ] # liftAff

  it "finds rows through ORM-style where operators" do
    withDbParsed do
      createUsersTable
      let exec = Client.execCount {}
      _ <- from usersTable # insert { name: "Ada", email: "ada@example.com", role: "admin" } # exec
      _ <- from usersTable # insert { name: "Grace", email: "grace@example.com", role: "member" } # exec
      rows <- Client.findWhere { role: Client.eq_ "admin" } usersTable
      map _.email rows `shouldEqual` [ "ada@example.com" ] # liftAff

  it "orders and limits rows through ORM-style helpers" do
    withDbParsed do
      createUsersTable
      let exec = Client.execCount {}
      _ <- from usersTable # insert { name: "Grace", email: "grace@example.com", role: "member" } # exec
      _ <- from usersTable # insert { name: "Ada", email: "ada@example.com", role: "admin" } # exec
      rows <- Client.findWhereLimited @"name" { email: Client.like_ "%@example.com" } Client.Asc { limit: 1, offset: 0 } usersTable
      map _.name rows `shouldEqual` [ "Ada" ] # liftAff

  it "counts rows through ORM-style helpers" do
    withDbParsed do
      createUsersTable
      let exec = Client.execCount {}
      _ <- from usersTable # insert { name: "Ada", email: "ada@example.com", role: "admin" } # exec
      _ <- from usersTable # insert { name: "Grace", email: "grace@example.com", role: "member" } # exec
      total <- Client.countAll usersTable
      admins <- Client.countWhere { role: Client.eq_ "admin" } usersTable
      total `shouldEqual` 2 # liftAff
      admins `shouldEqual` 1 # liftAff
