module Test.Main where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.SQLiteOm.ClientMutationSpec as ClientMutationSpec
import Test.SQLiteOm.ClientReadSpec as ClientReadSpec
import Test.SQLiteOm.ConnectionSpec as ConnectionSpec
import Test.SQLiteOm.RawBatchSpec as RawBatchSpec
import Test.SQLiteOm.RawSqlSpec as RawSqlSpec
import Test.SQLiteOm.TypedQuerySpec as TypedQuerySpec
import Test.SQLiteOm.TypedRawSpec as TypedRawSpec

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  ConnectionSpec.spec
  RawSqlSpec.spec
  RawBatchSpec.spec
  ClientReadSpec.spec
  ClientMutationSpec.spec
  TypedQuerySpec.spec
  TypedRawSpec.spec
