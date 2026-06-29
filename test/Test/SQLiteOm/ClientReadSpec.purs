module Test.SQLiteOm.ClientReadSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (createUsersTable, usersTable, withDb)
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.ClientOm (from, insert, returning, where_)

spec :: Spec Unit
spec = describe "typed ClientOm reads" do
  it "inserts a typed row and returns a typed projection" do
    withDb do
      createUsersTable
      created <- Client.runOne {} (from usersTable # insert { name: "Ada", email: "ada@example.com", role: "admin" } # returning @"name, email, role")
      liftAff $ created `shouldEqual` Just { name: "Ada", email: "ada@example.com", role: "admin" }

  it "selects typed rows with named parameters" do
    withDb do
      createUsersTable
      _ <- Client.execCount {} (from usersTable # insert { name: "Ada", email: "ada@example.com", role: "admin" })
      _ <- Client.execCount {} (from usersTable # insert { name: "Grace", email: "grace@example.com", role: "member" })
      rows <- Client.run { roleName: "admin" } (from usersTable # Client.select @"name, email, role" # where_ @"role = $roleName")
      liftAff $ rows `shouldEqual` [ { name: "Ada", email: "ada@example.com", role: "admin" } ]
