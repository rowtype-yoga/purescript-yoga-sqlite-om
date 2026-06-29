module Test.SQLiteOm.OrmSpec where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Foreign (MultipleErrors)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.SQLiteOm.Support (withDbParsed)
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Yoga.Om as Om
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.ClientOm (AutoIncrement, PrimaryKey, Table, createTableDDL, from, insert)
import Yoga.SQLite.Om as SQLiteOm
import Yoga.SQLite.SQLite as SQLite

spec :: Spec Unit
spec = describe "ORM-style SQLite ClientOm helpers" do
  it "finds all rows and orders by a typed column" do
    withMembers do
      rows <- Client.findOrdered @"name" Client.Asc membersTable
      map _.name rows `shouldEqual` [ "Ada", "Grace", "Linus" ] # liftAff

  it "finds one row with typed equality" do
    withMembers do
      row <- Client.findOneWhere { email: Client.eq_ "ada@example.com" } membersTable
      row `shouldEqual` Just { id: 1, name: "Ada", email: "ada@example.com", role: "admin", age: 41, nickname: Just "ace" } # liftAff

  it "filters with numeric comparisons" do
    withMembers do
      older <- Client.findWhere { age: Client.gt 40 } membersTable
      notTooOld <- Client.findWhere { age: Client.lte 41 } membersTable
      map _.name older `shouldEqual` [ "Ada", "Grace" ] # liftAff
      map _.name notTooOld `shouldEqual` [ "Ada", "Linus" ] # liftAff

  it "filters with inequality and negation" do
    withMembers do
      nonMembers <- Client.findWhere { role: Client.ne "member" } membersTable
      notAdmins <- Client.findWhere { role: Client.not_ (Client.eq_ "admin") } membersTable
      map _.name nonMembers `shouldEqual` [ "Ada", "Linus" ] # liftAff
      map _.name notAdmins `shouldEqual` [ "Grace", "Linus" ] # liftAff

  it "filters with LIKE, case-insensitive LIKE, and OR" do
    withMembers do
      exampleEmails <- Client.findWhere { email: Client.like_ "%@example.com" } membersTable
      lowerCaseName <- Client.findWhere { name: Client.ilike "ada" } membersTable
      elevated <- Client.findWhere { role: Client.or_ (Client.eq_ "admin") (Client.eq_ "owner") } membersTable
      map _.name exampleEmails `shouldEqual` [ "Ada", "Grace" ] # liftAff
      map _.email lowerCaseName `shouldEqual` [ "ada@example.com" ] # liftAff
      map _.name elevated `shouldEqual` [ "Ada", "Linus" ] # liftAff

  it "filters nullable columns" do
    withMembers do
      unnamed <- Client.findWhere { nickname: Client.isNull } membersTable
      named <- Client.findWhere { nickname: Client.isNotNull } membersTable
      map _.name unnamed `shouldEqual` [ "Grace" ] # liftAff
      map _.name named `shouldEqual` [ "Ada", "Linus" ] # liftAff

  it "pages after a typed cursor" do
    withMembers do
      page <- Client.findAfter @"id" 1 1 membersTable
      map _.name page.items `shouldEqual` [ "Grace" ] # liftAff
      page.hasMore `shouldEqual` true # liftAff

withMembers :: Om.Om { sqlite :: SQLite.Connection } (parseError :: MultipleErrors) Unit -> Aff Unit
withMembers body = withDbParsed do
  createMembersTable
  _ <- insertMember "Ada" "ada@example.com" "admin" 41 (Just "ace")
  _ <- insertMember "Grace" "grace@example.com" "member" 45 Nothing
  _ <- insertMember "Linus" "linus@example.net" "owner" 37 (Just "linux")
  body

type MembersTable = Table "members"
  ( id :: Int # PrimaryKey # AutoIncrement
  , name :: String
  , email :: String
  , role :: String
  , age :: Int
  , nickname :: Maybe String
  )

membersTable :: Proxy MembersTable
membersTable = Proxy

createMembersTable :: forall err. Om.Om { sqlite :: SQLite.Connection } err Unit
createMembersTable = do
  SQLiteOm.executeSimple (SQLite.SQL (createTableDDL @MembersTable)) # void

insertMember :: forall err. String -> String -> String -> Int -> Maybe String -> Om.Om { sqlite :: SQLite.Connection } err Int
insertMember name email role age nickname =
  Client.execCount {} (from membersTable # insert { name, email, role, age, nickname })
