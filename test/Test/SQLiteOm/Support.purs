module Test.SQLiteOm.Support
  ( UsersTable
  , usersTable
  , withDb
  , withDbParsed
  , createUsersTable
  , insertUser
  ) where

import Prelude

import Effect.Aff (Aff, throwError)
import Effect.Exception as Exception
import Effect.Class (liftEffect)
import Foreign (MultipleErrors)
import Type.Function (type (#))
import Type.Proxy (Proxy(..))
import Yoga.Om as Om
import Yoga.SQLite.ClientOm (AutoIncrement, PrimaryKey, Table, createTableDDL, from, insert)
import Yoga.SQLite.ClientOm as Client
import Yoga.SQLite.Om as SQLiteOm
import Yoga.SQLite.SQLite as SQLite

type UsersTable = Table "users"
  ( id :: Int # PrimaryKey # AutoIncrement
  , name :: String
  , email :: String
  , role :: String
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

withDb :: Om.Om { sqlite :: SQLite.Connection } () Unit -> Aff Unit
withDb om = do
  conn <- liftEffect $ SQLite.sqlite { url: ":memory:" }
  _ <- Om.runOm { sqlite: conn } { exception: throwError } om
  SQLite.close conn # liftEffect

withDbParsed :: Om.Om { sqlite :: SQLite.Connection } (parseError :: MultipleErrors) Unit -> Aff Unit
withDbParsed om = do
  conn <- liftEffect $ SQLite.sqlite { url: ":memory:" }
  _ <- Om.runOm { sqlite: conn } { exception: throwError, parseError: show >>> Exception.error >>> throwError } om
  SQLite.close conn # liftEffect

createUsersTable :: forall err. Om.Om { sqlite :: SQLite.Connection } err Unit
createUsersTable = do
  SQLiteOm.executeSimple (SQLite.SQL (createTableDDL @UsersTable)) # void

insertUser :: forall err. String -> String -> String -> Om.Om { sqlite :: SQLite.Connection } err Int
insertUser name email role =
  Client.execCount {} (from usersTable # insert { name, email, role })
