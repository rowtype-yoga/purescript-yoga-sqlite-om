module Yoga.SQLite.ClientOm
  ( run
  , runOne
  , exec
  , execCount
  , Gt
  , Gte
  , Lt
  , Lte
  , Ne
  , Like
  , ILike
  , IsNull
  , IsNotNull
  , Eq
  , Or
  , Not
  , gt
  , gte
  , lt
  , lte
  , ne
  , like_
  , ilike
  , isNull
  , isNotNull
  , eq_
  , or_
  , not_
  , class ExtractWhereType
  , class WhereFieldSQL
  , whereFieldSQLFragment
  , class ValidateWhereColumnsRL
  , class OmWhereClauseRL
  , whereClauseFragments
  , class TableRow
  , class InsertableRow
  , class FindPrimaryKey
  , OrderDir(..)
  , PageRecord
  , findAll
  , findWhere
  , findOneWhere
  , findOrdered
  , findWhereOrdered
  , findWhereLimited
  , findAfter
  , findAfterWithKey
  , create
  , createReturningAll
  , updateWhere
  , deleteWhere
  , countWhere
  , countAll
  , module Schema
  ) where

import Prelude

import Data.Array (head, intercalate, length, mapWithIndex, take)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Effect.Aff.Class (liftAff)
import Foreign (Foreign, MultipleErrors)
import Prim.Boolean (False, True)
import Prim.Row (class Cons) as Row
import Prim.RowList as RL
import Prim.RowList (class RowToList)
import Prim.TypeError (class Fail, Text)
import Record as Record
import Type.Proxy (Proxy(..))
import Type.RowList (class ListToRow)
import Yoga.JSON (class ReadForeign)
import Yoga.JSON as JSON
import Yoga.Om as Om
import Yoga.SQLite.Schema as Schema
import Yoga.SQLite.Schema (class ExtractType, class FieldToSQLiteValue, class InsertableColumnsRL, class ParamsToArray, class RecordValuesRL, class SetClauseRL, class StripColumnsRL, AutoIncrement, Default, ForeignKey, Nullable, PrimaryKey, Table, Unique, fieldToSQLiteValue, setClauseRL)
import Yoga.SQLite.SQLite (Connection)
import Yoga.SQLite.SQLite as SQLite

run
  :: forall tables result params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => { | params }
  -> Schema.Q tables result params stage
  -> Om.Om { sqlite :: Connection | r } err (Array { | result })
run params q = do
  { sqlite } <- Om.ask
  Schema.runQuery sqlite params q # liftAff

runOne
  :: forall tables result params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => ReadForeign { | result }
  => { | params }
  -> Schema.Q tables result params stage
  -> Om.Om { sqlite :: Connection | r } err (Maybe { | result })
runOne params q = do
  { sqlite } <- Om.ask
  Schema.runQueryOne sqlite params q # liftAff

exec
  :: forall tables params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => { | params }
  -> Schema.Q tables () params stage
  -> Om.Om { sqlite :: Connection | r } err Unit
exec params q = do
  { sqlite } <- Om.ask
  Schema.runExecute sqlite params q # liftAff # void

execCount
  :: forall tables params paramsRL stage r err
   . RowToList params paramsRL
  => ParamsToArray paramsRL params
  => { | params }
  -> Schema.Q tables () params stage
  -> Om.Om { sqlite :: Connection | r } err Int
execCount params q = do
  { sqlite } <- Om.ask
  Schema.runExecute sqlite params q # liftAff

data Gt a = Gt a
data Gte a = Gte a
data Lt a = Lt a
data Lte a = Lte a
data Ne a = Ne a
data Like a = Like a
data ILike a = ILike a
data IsNull = IsNull
data IsNotNull = IsNotNull
data Eq a = Eq a
data Or a b = Or a b
data Not a = Not a

gt :: forall a. a -> Gt a
gt = Gt

gte :: forall a. a -> Gte a
gte = Gte

lt :: forall a. a -> Lt a
lt = Lt

lte :: forall a. a -> Lte a
lte = Lte

ne :: forall a. a -> Ne a
ne = Ne

like_ :: forall a. a -> Like a
like_ = Like

ilike :: forall a. a -> ILike a
ilike = ILike

isNull :: IsNull
isNull = IsNull

isNotNull :: IsNotNull
isNotNull = IsNotNull

eq_ :: forall a. a -> Eq a
eq_ = Eq

or_ :: forall a b. a -> b -> Or a b
or_ = Or

not_ :: forall a. a -> Not a
not_ = Not

class ExtractWhereType :: Type -> Type -> Constraint
class ExtractWhereType wrapped baseType | wrapped -> baseType

instance ExtractWhereType (Gt a) a
else instance ExtractWhereType (Gte a) a
else instance ExtractWhereType (Lt a) a
else instance ExtractWhereType (Lte a) a
else instance ExtractWhereType (Ne a) a
else instance ExtractWhereType (Like a) a
else instance ExtractWhereType (ILike a) a
else instance ExtractWhereType (Eq a) a
else instance ExtractWhereType a b => ExtractWhereType (Not a) b
else instance ExtractWhereType IsNull (Maybe a)
else instance ExtractWhereType IsNotNull (Maybe a)
else instance ExtractWhereType a c => ExtractWhereType (Or a b) c
else instance ExtractWhereType a a

class WhereFieldSQL :: Type -> Constraint
class WhereFieldSQL a where
  whereFieldSQLFragment :: a -> String -> Int -> { sql :: String, values :: Array SQLite.SQLiteValue, nextIdx :: Int }

instance FieldToSQLiteValue a => WhereFieldSQL (Gt a) where
  whereFieldSQLFragment (Gt a) col idx =
    { sql: col <> " > ?" <> show idx, values: [ fieldToSQLiteValue a ], nextIdx: idx + 1 }

else instance FieldToSQLiteValue a => WhereFieldSQL (Gte a) where
  whereFieldSQLFragment (Gte a) col idx =
    { sql: col <> " >= ?" <> show idx, values: [ fieldToSQLiteValue a ], nextIdx: idx + 1 }

else instance FieldToSQLiteValue a => WhereFieldSQL (Lt a) where
  whereFieldSQLFragment (Lt a) col idx =
    { sql: col <> " < ?" <> show idx, values: [ fieldToSQLiteValue a ], nextIdx: idx + 1 }

else instance FieldToSQLiteValue a => WhereFieldSQL (Lte a) where
  whereFieldSQLFragment (Lte a) col idx =
    { sql: col <> " <= ?" <> show idx, values: [ fieldToSQLiteValue a ], nextIdx: idx + 1 }

else instance FieldToSQLiteValue a => WhereFieldSQL (Ne a) where
  whereFieldSQLFragment (Ne a) col idx =
    { sql: col <> " != ?" <> show idx, values: [ fieldToSQLiteValue a ], nextIdx: idx + 1 }

else instance FieldToSQLiteValue a => WhereFieldSQL (Like a) where
  whereFieldSQLFragment (Like a) col idx =
    { sql: col <> " LIKE ?" <> show idx, values: [ fieldToSQLiteValue a ], nextIdx: idx + 1 }

else instance FieldToSQLiteValue a => WhereFieldSQL (ILike a) where
  whereFieldSQLFragment (ILike a) col idx =
    { sql: "LOWER(" <> col <> ") LIKE LOWER(?" <> show idx <> ")", values: [ fieldToSQLiteValue a ], nextIdx: idx + 1 }

else instance WhereFieldSQL IsNull where
  whereFieldSQLFragment _ col idx =
    { sql: col <> " IS NULL", values: [], nextIdx: idx }

else instance WhereFieldSQL IsNotNull where
  whereFieldSQLFragment _ col idx =
    { sql: col <> " IS NOT NULL", values: [], nextIdx: idx }

else instance FieldToSQLiteValue a => WhereFieldSQL (Eq a) where
  whereFieldSQLFragment (Eq a) col idx =
    { sql: col <> " = ?" <> show idx, values: [ fieldToSQLiteValue a ], nextIdx: idx + 1 }

else instance (WhereFieldSQL a, WhereFieldSQL b) => WhereFieldSQL (Or a b) where
  whereFieldSQLFragment (Or a b) col idx = do
    let ra = whereFieldSQLFragment a col idx
    let rb = whereFieldSQLFragment b col ra.nextIdx
    { sql: "(" <> ra.sql <> " OR " <> rb.sql <> ")", values: ra.values <> rb.values, nextIdx: rb.nextIdx }

else instance WhereFieldSQL a => WhereFieldSQL (Not a) where
  whereFieldSQLFragment (Not a) col idx = do
    let r = whereFieldSQLFragment a col idx
    { sql: "NOT (" <> r.sql <> ")", values: r.values, nextIdx: r.nextIdx }

else instance FieldToSQLiteValue a => WhereFieldSQL a where
  whereFieldSQLFragment a col idx =
    { sql: col <> " = ?" <> show idx, values: [ fieldToSQLiteValue a ], nextIdx: idx + 1 }

class ValidateWhereColumnsRL :: RL.RowList Type -> Row Type -> Constraint
class ValidateWhereColumnsRL rl cols

instance ValidateWhereColumnsRL RL.Nil cols
instance
  ( Row.Cons name entry rest cols
  , ExtractType entry colType
  , ExtractWhereType whereType colType
  , ValidateWhereColumnsRL tail cols
  ) =>
  ValidateWhereColumnsRL (RL.Cons name whereType tail) cols

class OmWhereClauseRL :: RL.RowList Type -> Row Type -> Constraint
class OmWhereClauseRL rl row where
  whereClauseFragments :: Proxy rl -> { | row } -> Int -> { sql :: Array String, values :: Array SQLite.SQLiteValue, nextIdx :: Int }

instance OmWhereClauseRL RL.Nil row where
  whereClauseFragments _ _ idx = { sql: [], values: [], nextIdx: idx }

instance
  ( IsSymbol name
  , Row.Cons name typ rest row
  , WhereFieldSQL typ
  , OmWhereClauseRL tail row
  ) =>
  OmWhereClauseRL (RL.Cons name typ tail) row where
  whereClauseFragments _ rec idx = do
    let value = Record.get (Proxy :: Proxy name) rec
    let col = reflectSymbol (Proxy :: Proxy name)
    let field = whereFieldSQLFragment value col idx
    let rest = whereClauseFragments (Proxy :: Proxy tail) rec field.nextIdx
    { sql: [ field.sql ] <> rest.sql, values: field.values <> rest.values, nextIdx: rest.nextIdx }

class TableRow :: Type -> Row Type -> Constraint
class TableRow table row | table -> row

instance
  ( RowToList cols rl
  , StripColumnsRL rl outRL
  , ListToRow outRL row
  ) =>
  TableRow (Table name cols) row

class InsertableRow :: Type -> Row Type -> Constraint
class InsertableRow table row | table -> row

instance
  ( RowToList cols rl
  , InsertableColumnsRL rl insertRL
  , ListToRow insertRL row
  ) =>
  InsertableRow (Table name cols) row

class HasPrimaryKeyConstraint :: Type -> Boolean -> Constraint
class HasPrimaryKeyConstraint a result | a -> result

instance HasPrimaryKeyConstraint (PrimaryKey a) True
else instance HasPrimaryKeyConstraint a result => HasPrimaryKeyConstraint (AutoIncrement a) result
else instance HasPrimaryKeyConstraint a result => HasPrimaryKeyConstraint (Unique a) result
else instance HasPrimaryKeyConstraint a result => HasPrimaryKeyConstraint (Default s a) result
else instance HasPrimaryKeyConstraint a result => HasPrimaryKeyConstraint (ForeignKey t r c a) result
else instance HasPrimaryKeyConstraint a result => HasPrimaryKeyConstraint (Nullable a) result
else instance HasPrimaryKeyConstraint a False

class FindPrimaryKeyRL :: RL.RowList Type -> Symbol -> Type -> Constraint
class FindPrimaryKeyRL rl colName colType | rl -> colName colType

instance Fail (Text "Table has no primary key column") => FindPrimaryKeyRL RL.Nil "" Unit
instance
  ( HasPrimaryKeyConstraint entry isPK
  , FindPrimaryKeyDecide isPK name entry tail colName colType
  ) =>
  FindPrimaryKeyRL (RL.Cons name entry tail) colName colType

class FindPrimaryKeyDecide :: Boolean -> Symbol -> Type -> RL.RowList Type -> Symbol -> Type -> Constraint
class FindPrimaryKeyDecide isPK name entry tail colName colType | isPK name entry tail -> colName colType

instance ExtractType entry colType => FindPrimaryKeyDecide True name entry tail name colType
instance FindPrimaryKeyRL tail colName colType => FindPrimaryKeyDecide False name entry tail colName colType

class FindPrimaryKey :: Type -> Symbol -> Type -> Constraint
class FindPrimaryKey table colName colType | table -> colName colType

instance
  ( RowToList cols rl
  , FindPrimaryKeyRL rl colName colType
  ) =>
  FindPrimaryKey (Table name cols) colName colType

data Page a = Page { items :: Array { | a }, hasMore :: Boolean }

type PageRecord a = { items :: Array { | a }, hasMore :: Boolean }

data OrderDir = Asc | Desc

orderDirSQL :: OrderDir -> String
orderDirSQL Asc = "ASC"
orderDirSQL Desc = "DESC"

parseRow :: forall a ctx err. ReadForeign a => Foreign -> Om.Om ctx (parseError :: MultipleErrors | err) a
parseRow row = case (JSON.read row :: Either _ a) of
  Left errors -> Om.throw { parseError: errors }
  Right value -> pure value

findAll
  :: forall name cols row r err
   . IsSymbol name
  => TableRow (Table name cols) row
  => ReadForeign { | row }
  => Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) (Array { | row })
findAll _ = do
  { sqlite } <- Om.ask
  result <- SQLite.query (SQLite.SQL sql) [] sqlite # liftAff
  traverse parseRow result.rows
  where
  sql = "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name)

findWhere
  :: forall name cols row whereRow whereRL r err
   . IsSymbol name
  => TableRow (Table name cols) row
  => RowToList whereRow whereRL
  => ValidateWhereColumnsRL whereRL cols
  => OmWhereClauseRL whereRL whereRow
  => ReadForeign { | row }
  => { | whereRow }
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) (Array { | row })
findWhere whereRec _ = do
  { sqlite } <- Om.ask
  result <- SQLite.query (SQLite.SQL sql) whereResult.values sqlite # liftAff
  traverse parseRow result.rows
  where
  whereResult = whereClauseFragments (Proxy :: Proxy whereRL) whereRec 1
  sql = "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name)
    <> " WHERE " <> intercalate " AND " whereResult.sql

findOneWhere
  :: forall name cols row whereRow whereRL r err
   . IsSymbol name
  => TableRow (Table name cols) row
  => RowToList whereRow whereRL
  => ValidateWhereColumnsRL whereRL cols
  => OmWhereClauseRL whereRL whereRow
  => ReadForeign { | row }
  => { | whereRow }
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) (Maybe { | row })
findOneWhere whereRec table = do
  rows <- findWhere whereRec table
  pure (head rows)

findOrdered
  :: forall @col name cols colEntry rest row r err
   . IsSymbol name
  => IsSymbol col
  => Row.Cons col colEntry rest cols
  => TableRow (Table name cols) row
  => ReadForeign { | row }
  => OrderDir
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) (Array { | row })
findOrdered dir _ = do
  { sqlite } <- Om.ask
  result <- SQLite.query (SQLite.SQL sql) [] sqlite # liftAff
  traverse parseRow result.rows
  where
  sql = "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name)
    <> " ORDER BY " <> reflectSymbol (Proxy :: Proxy col) <> " " <> orderDirSQL dir

findWhereOrdered
  :: forall @col name cols colEntry rest row whereRow whereRL r err
   . IsSymbol name
  => IsSymbol col
  => Row.Cons col colEntry rest cols
  => TableRow (Table name cols) row
  => RowToList whereRow whereRL
  => ValidateWhereColumnsRL whereRL cols
  => OmWhereClauseRL whereRL whereRow
  => ReadForeign { | row }
  => { | whereRow }
  -> OrderDir
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) (Array { | row })
findWhereOrdered whereRec dir _ = do
  { sqlite } <- Om.ask
  result <- SQLite.query (SQLite.SQL sql) whereResult.values sqlite # liftAff
  traverse parseRow result.rows
  where
  whereResult = whereClauseFragments (Proxy :: Proxy whereRL) whereRec 1
  sql = "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name)
    <> " WHERE " <> intercalate " AND " whereResult.sql
    <> " ORDER BY " <> reflectSymbol (Proxy :: Proxy col) <> " " <> orderDirSQL dir

findWhereLimited
  :: forall @col name cols colEntry rest row whereRow whereRL r err
   . IsSymbol name
  => IsSymbol col
  => Row.Cons col colEntry rest cols
  => TableRow (Table name cols) row
  => RowToList whereRow whereRL
  => ValidateWhereColumnsRL whereRL cols
  => OmWhereClauseRL whereRL whereRow
  => ReadForeign { | row }
  => { | whereRow }
  -> OrderDir
  -> { limit :: Int, offset :: Int }
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) (Array { | row })
findWhereLimited whereRec dir opts _ = do
  { sqlite } <- Om.ask
  result <- SQLite.query (SQLite.SQL sql) values sqlite # liftAff
  traverse parseRow result.rows
  where
  whereResult = whereClauseFragments (Proxy :: Proxy whereRL) whereRec 1
  limitIdx = whereResult.nextIdx
  offsetIdx = whereResult.nextIdx + 1
  sql = "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name)
    <> " WHERE " <> intercalate " AND " whereResult.sql
    <> " ORDER BY " <> reflectSymbol (Proxy :: Proxy col) <> " " <> orderDirSQL dir
    <> " LIMIT ?" <> show limitIdx
    <> " OFFSET ?" <> show offsetIdx
  values = whereResult.values <> [ fieldToSQLiteValue opts.limit, fieldToSQLiteValue opts.offset ]

findAfter
  :: forall @col name cols colEntry rest row colType r err
   . IsSymbol name
  => IsSymbol col
  => Row.Cons col colEntry rest cols
  => ExtractType colEntry colType
  => FieldToSQLiteValue colType
  => TableRow (Table name cols) row
  => ReadForeign { | row }
  => colType
  -> Int
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) (PageRecord row)
findAfter cursor limit _ = do
  { sqlite } <- Om.ask
  result <- SQLite.query (SQLite.SQL sql) [ fieldToSQLiteValue cursor, fieldToSQLiteValue (limit + 1) ] sqlite # liftAff
  allRows <- traverse parseRow result.rows
  pure { items: take limit allRows, hasMore: length allRows > limit }
  where
  sql = "SELECT * FROM " <> reflectSymbol (Proxy :: Proxy name)
    <> " WHERE " <> reflectSymbol (Proxy :: Proxy col) <> " > ?1"
    <> " ORDER BY " <> reflectSymbol (Proxy :: Proxy col) <> " ASC"
    <> " LIMIT ?2"

findAfterWithKey
  :: forall @col @key name cols colEntry keyEntry colRest keyRest row colType keyType r err
   . IsSymbol name
  => IsSymbol col
  => IsSymbol key
  => Row.Cons col colEntry colRest cols
  => Row.Cons key keyEntry keyRest cols
  => ExtractType colEntry colType
  => ExtractType keyEntry keyType
  => FieldToSQLiteValue colType
  => FieldToSQLiteValue keyType
  => TableRow (Table name cols) row
  => ReadForeign { | row }
  => { cursor :: colType, key :: keyType }
  -> Int
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) (PageRecord row)
findAfterWithKey cursor limit _ = do
  { sqlite } <- Om.ask
  result <- SQLite.query (SQLite.SQL sql) [ fieldToSQLiteValue cursor.cursor, fieldToSQLiteValue cursor.cursor, fieldToSQLiteValue cursor.key, fieldToSQLiteValue (limit + 1) ] sqlite # liftAff
  allRows <- traverse parseRow result.rows
  pure { items: take limit allRows, hasMore: length allRows > limit }
  where
  table = reflectSymbol (Proxy :: Proxy name)
  orderColumn = reflectSymbol (Proxy :: Proxy col)
  keyColumn = reflectSymbol (Proxy :: Proxy key)
  sql = "SELECT * FROM " <> table
    <> " WHERE (" <> orderColumn <> " > ?1 OR (" <> orderColumn <> " = ?2 AND " <> keyColumn <> " > ?3))"
    <> " ORDER BY " <> orderColumn <> " ASC, " <> keyColumn <> " ASC"
    <> " LIMIT ?4"

create
  :: forall name cols tables row rowRL r err
   . IsSymbol name
  => Row.Cons name cols () tables
  => RowToList row rowRL
  => InsertableRow (Table name cols) row
  => RecordValuesRL rowRL row
  => Schema.ColumnNamesRL rowRL
  => { | row }
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } err Int
create row _ = do
  { sqlite } <- Om.ask
  SQLite.execute (SQLite.SQL sql) values sqlite # liftAff
  where
  tableName = reflectSymbol (Proxy :: Proxy name)
  colNames = Schema.columnNamesRL (Proxy :: Proxy rowRL)
  placeholders = mapWithIndex (\i _ -> "?" <> show (i + 1)) colNames
  sql =
    if length colNames == 0 then "INSERT INTO " <> tableName <> " DEFAULT VALUES"
    else "INSERT INTO " <> tableName <> " (" <> intercalate ", " colNames <> ") VALUES (" <> intercalate ", " placeholders <> ")"
  values = Schema.recordValuesRL (Proxy :: Proxy rowRL) row

createReturningAll
  :: forall name cols tables row rowRL result r err
   . IsSymbol name
  => Row.Cons name cols () tables
  => RowToList row rowRL
  => InsertableRow (Table name cols) row
  => RecordValuesRL rowRL row
  => Schema.ColumnNamesRL rowRL
  => TableRow (Table name cols) result
  => ReadForeign { | result }
  => { | row }
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) (Maybe { | result })
createReturningAll row _ = do
  { sqlite } <- Om.ask
  maybeRow <- SQLite.queryOne (SQLite.SQL sql) values sqlite # liftAff
  traverse parseRow maybeRow
  where
  tableName = reflectSymbol (Proxy :: Proxy name)
  colNames = Schema.columnNamesRL (Proxy :: Proxy rowRL)
  placeholders = mapWithIndex (\i _ -> "?" <> show (i + 1)) colNames
  insertSql =
    if length colNames == 0 then "INSERT INTO " <> tableName <> " DEFAULT VALUES"
    else "INSERT INTO " <> tableName <> " (" <> intercalate ", " colNames <> ") VALUES (" <> intercalate ", " placeholders <> ")"
  sql = insertSql <> " RETURNING *"
  values = Schema.recordValuesRL (Proxy :: Proxy rowRL) row

updateWhere
  :: forall name cols setRow setRL whereRow whereRL r err
   . IsSymbol name
  => RowToList setRow setRL
  => RowToList whereRow whereRL
  => Schema.ValidateSetColumnsRL setRL cols
  => SetClauseRL setRL
  => RecordValuesRL setRL setRow
  => ValidateWhereColumnsRL whereRL cols
  => OmWhereClauseRL whereRL whereRow
  => { | whereRow }
  -> { | setRow }
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } err Int
updateWhere whereRec setRec _ = do
  { sqlite } <- Om.ask
  SQLite.execute (SQLite.SQL sql) (setValues <> whereResult.values) sqlite # liftAff
  where
  tableName = reflectSymbol (Proxy :: Proxy name)
  setValues = Schema.recordValuesRL (Proxy :: Proxy setRL) setRec
  whereResult = whereClauseFragments (Proxy :: Proxy whereRL) whereRec (length setValues + 1)
  sql = "UPDATE " <> tableName
    <> " SET " <> intercalate ", " (setClauseRL (Proxy :: Proxy setRL) 1)
    <> " WHERE " <> intercalate " AND " whereResult.sql

deleteWhere
  :: forall name cols whereRow whereRL r err
   . IsSymbol name
  => RowToList whereRow whereRL
  => ValidateWhereColumnsRL whereRL cols
  => OmWhereClauseRL whereRL whereRow
  => { | whereRow }
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } err Int
deleteWhere whereRec _ = do
  { sqlite } <- Om.ask
  SQLite.execute (SQLite.SQL sql) whereResult.values sqlite # liftAff
  where
  whereResult = whereClauseFragments (Proxy :: Proxy whereRL) whereRec 1
  sql = "DELETE FROM " <> reflectSymbol (Proxy :: Proxy name)
    <> " WHERE " <> intercalate " AND " whereResult.sql

countWhere
  :: forall name cols whereRow whereRL r err
   . IsSymbol name
  => RowToList whereRow whereRL
  => ValidateWhereColumnsRL whereRL cols
  => OmWhereClauseRL whereRL whereRow
  => { | whereRow }
  -> Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) Int
countWhere whereRec _ = do
  { sqlite } <- Om.ask
  maybeRow <- SQLite.queryOne (SQLite.SQL sql) whereResult.values sqlite # liftAff
  countFrom maybeRow
  where
  whereResult = whereClauseFragments (Proxy :: Proxy whereRL) whereRec 1
  sql = "SELECT COUNT(*) AS result FROM " <> reflectSymbol (Proxy :: Proxy name)
    <> " WHERE " <> intercalate " AND " whereResult.sql

countAll
  :: forall name cols r err
   . IsSymbol name
  => Proxy (Table name cols)
  -> Om.Om { sqlite :: Connection | r } (parseError :: MultipleErrors | err) Int
countAll _ = do
  { sqlite } <- Om.ask
  maybeRow <- SQLite.queryOne (SQLite.SQL sql) [] sqlite # liftAff
  countFrom maybeRow
  where
  sql = "SELECT COUNT(*) AS result FROM " <> reflectSymbol (Proxy :: Proxy name)

countFrom :: forall ctx err. Maybe Foreign -> Om.Om ctx (parseError :: MultipleErrors | err) Int
countFrom = case _ of
  Nothing -> pure 0
  Just row -> _.result <$> (parseRow row :: Om.Om ctx (parseError :: MultipleErrors | err) { result :: Int })
