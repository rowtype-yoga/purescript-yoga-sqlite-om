module Test.SQLiteOm.ClientReadSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (createFactsTable, factsTable, withDb)
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.ClientOm (from, insert, returning, where_)

spec :: Spec Unit
spec = describe "typed ClientOm reads" do
  it "inserts a typed row and returns a typed projection" do
    withDb do
      createFactsTable
      created <- Client.runOne {} (from factsTable # insert { module: "Data.Foldable", ident: "elem", ps_type: "forall a. a" } # returning @"module, ident, ps_type")
      liftAff $ created `shouldEqual` Just { module: "Data.Foldable", ident: "elem", ps_type: "forall a. a" }

  it "selects typed rows with named parameters" do
    withDb do
      createFactsTable
      _ <- Client.execCount {} (from factsTable # insert { module: "Data.Foldable", ident: "elem", ps_type: "forall a. a" })
      _ <- Client.execCount {} (from factsTable # insert { module: "Data.Array", ident: "foldl", ps_type: "forall a b. (b -> a -> b) -> b -> Array a -> b" })
      rows <- Client.run { moduleName: "Data.Foldable" } (from factsTable # Client.select @"module, ident, ps_type" # where_ @"module = $moduleName")
      liftAff $ rows `shouldEqual` [ { module: "Data.Foldable", ident: "elem", ps_type: "forall a. a" } ]
