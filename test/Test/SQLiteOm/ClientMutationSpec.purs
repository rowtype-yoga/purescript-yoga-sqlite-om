module Test.SQLiteOm.ClientMutationSpec where

import Prelude

import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (createUsersTable, usersTable, withDb)
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.ClientOm (from, insert, where_)

spec :: Spec Unit
spec = describe "typed ClientOm mutations" do
  it "reports affected rows for typed inserts" do
    withDb do
      createUsersTable
      first <- Client.execCount {} (from usersTable # insert { name: "Ada", email: "ada@example.com", role: "admin" })
      second <- Client.execCount {} (from usersTable # insert { name: "Grace", email: "grace@example.com", role: "member" })
      first `shouldEqual` 1 # liftAff
      second `shouldEqual` 1 # liftAff

  it "runs Unit-returning typed mutations" do
    withDb do
      createUsersTable
      Client.exec {} (from usersTable # insert { name: "Ada", email: "ada@example.com", role: "admin" })
      rows <- Client.run { email: "ada@example.com" } (from usersTable # Client.select @"name, email, role" # where_ @"email = $email")
      map _.name rows `shouldEqual` [ "Ada" ] # liftAff
