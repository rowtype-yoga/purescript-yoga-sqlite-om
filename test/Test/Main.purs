module Test.Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Yoga.Om as Om
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.ClientOm (AutoIncrement, PrimaryKey, Table, createTableDDL, from, insert, returning, where_)
import Yoga.SQLite.Om as SQLiteOm
import Yoga.SQLite.SQLite as SQLite

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] spec

spec :: Spec Unit
spec = describe "SQLite Om" do
  it "runs raw SQLite operations from Om context" do
    withDb do
      _ <- SQLiteOm.executeSimple (SQLite.SQL "CREATE TABLE facts (module TEXT NOT NULL, ident TEXT NOT NULL, ps_type TEXT NOT NULL, PRIMARY KEY (module, ident))")
      _ <- SQLiteOm.execute (SQLite.SQL "INSERT INTO facts (module, ident, ps_type) VALUES (?1, ?2, ?3)") (SQLite.params [ "Data.Foldable", "elem", "forall a. a" ])
      result <- SQLiteOm.query (SQLite.SQL "SELECT ps_type FROM facts WHERE module = ?1 AND ident = ?2") (SQLite.params [ "Data.Foldable", "elem" ])
      liftAff $ Array.length result.rows `shouldEqual` 1
      liftAff $ result.columns `shouldEqual` [ "ps_type" ]

  it "runs typed schema queries from Om context" do
    withDb do
      _ <- SQLiteOm.executeSimple (SQLite.SQL (createTableDDL @FactsTable))
      created <- Client.runOne {} (from factsTable # insert { module: "Data.Foldable", ident: "elem", ps_type: "forall a. a" } # returning @"module, ident, ps_type")
      liftAff $ created `shouldEqual` Just { module: "Data.Foldable", ident: "elem", ps_type: "forall a. a" }
      rows <- Client.run { ident: "elem" } (from factsTable # Client.select @"module, ident, ps_type" # where_ @"ident = $ident")
      liftAff $ rows `shouldEqual` [ { module: "Data.Foldable", ident: "elem", ps_type: "forall a. a" } ]

withDb :: Om.Om { sqlite :: SQLite.Connection } () Unit -> Aff Unit
withDb om = do
  conn <- liftEffect $ SQLite.sqlite { url: ":memory:" }
  _ <- Om.runOm { sqlite: conn } { exception: throwError } om
  SQLite.close conn # liftEffect

type FactsTable = Table "facts"
  ( id :: Int # PrimaryKey # AutoIncrement
  , module :: String
  , ident :: String
  , ps_type :: String
  )

factsTable :: Proxy FactsTable
factsTable = Proxy
