module Test.SQLiteOm.ClientMutationSpec where

import Prelude

import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (createFactsTable, factsTable, withDb)
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.ClientOm (from, insert, where_)

spec :: Spec Unit
spec = describe "typed ClientOm mutations" do
  it "reports affected rows for typed inserts" do
    withDb do
      createFactsTable
      first <- Client.execCount {} (from factsTable # insert { module: "Data.Semigroup", ident: "append", ps_type: "forall a. Semigroup a => a -> a -> a" })
      second <- Client.execCount {} (from factsTable # insert { module: "Data.Semigroup", ident: "stimes", ps_type: "forall a. Semigroup a => Int -> a -> a" })
      liftAff $ first `shouldEqual` 1
      liftAff $ second `shouldEqual` 1

  it "runs Unit-returning typed mutations" do
    withDb do
      createFactsTable
      Client.exec {} (from factsTable # insert { module: "Data.Semigroup", ident: "append", ps_type: "forall a. Semigroup a => a -> a -> a" })
      rows <- Client.run { ident: "append" } (from factsTable # Client.select @"module, ident, ps_type" # where_ @"ident = $ident")
      liftAff $ map _.module rows `shouldEqual` [ "Data.Semigroup" ]
