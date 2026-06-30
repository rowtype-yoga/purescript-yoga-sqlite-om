module Test.SQLiteOm.ConnectionSpec where

import Prelude

import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (withDb)
import Yoga.Om as Om
import Yoga.SQLite.SQLite as SQLite

spec :: Spec Unit
spec = describe "SQLite connection service" do
  it "provides an open SQLite connection through Om context" do
    withDb do
      { sqlite } <- Om.ask
      isOpen <- SQLite.closed sqlite # liftEffect
      isOpen `shouldEqual` false # liftAff

  it "can ping the provided in-memory database" do
    withDb do
      { sqlite } <- Om.ask
      alive <- SQLite.ping sqlite # liftAff
      alive `shouldEqual` true # liftAff
