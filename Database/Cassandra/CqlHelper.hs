{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : Database.Cassandra.CqlHelper
Description : helper types/functions that make it easier to work with data types and Cassandra.
Copyright   : (c) Tim Stewart, 2014
License     : BSD3
Maintainer  : tim.j.stewart@gmail.com
Stability   : experimental
-}

module Database.Cassandra.CqlHelper
       (
         -- * Connection
         newSystemPool,

         -- * Keyspace
         createKeyspace,
         KeyspaceName,
         KeyspaceResult,

         -- * Table
         TableDef(..),
         createTable,

         -- * Column
         Column(..),

         -- * Row
         Row(..),
         Readable(..),
         Writable(..),
         Destroyable(..),
         RowLimit,

         -- * Miscellaneous
         ReplicationFactor,
         genId
       ) where

import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.CatchIO (catch)
import Data.UUID (UUID(..))
import System.Random (randomIO)
import Database.Cassandra.CQL
import Data.String (fromString)
import qualified Data.List as L

-- | generates a UUID in the Cas Monad
genId :: Cas UUID
genId = liftIO randomIO

-- | the result of a keyspace operation once it's been wrapped in toEither
type KeyspaceResult = Either CassandraException (Change, Keyspace, Table)

-- | specifies a column in a Cassandra database
data Column = Column {
  columnName :: String,
  columnType :: CType
  } deriving (Show, Eq)

-- | a Cassandra table definition
data TableDef = TableDef {
  tableName :: String,
  tableColumns :: [Column],
  -- ^the table's columns that are not primary keys
  tablePrimaryKey :: Column
  -- ^the table's primary key column
  } deriving (Show, Eq)

-- | a limit on how many rows should be returned
type RowLimit = Int

-- | an instance of Row can be serialized to a tuple and deserialized
-- from a tuple
class Row r where
  -- | the tuple type (e.g. (Int, Int), (UUID, Text, Text), etc.)
  type Tuple r :: *
  -- | returns a tuple of the Row's values
  toTuple :: r -> Tuple r
  -- | creates an r value from a Tuple r value
  fromTuple :: Tuple r -> r

-- | an instance of Readable can perform read operations on the
-- database
class Row r => Readable r where
  -- | returns all records in table (unless there is a limit)
  findAll :: Consistency -> Maybe RowLimit -> Cas [r]
  -- | returns the record whose id is equal to the supplied id
  findById :: (Show a, CasType a) => Consistency -> a -> Cas (Maybe r)

-- | an instance of Writable can perform updates on the database.
-- TODO: add delete
class Row r => Writable r where
  -- | inserts a single record into the table
  insert :: Consistency -> r -> Cas ()

-- | an instance of Destroyable can delete records from the databse
class Row r => Destroyable r where
  -- | inserts a single record into the table
  delete :: Consistency -> r -> Cas ()

-- | returns a new Cassandra Server Pool for the system keyspace
newSystemPool :: [Server] -> IO Pool
newSystemPool s = newPool s "system"

type KeyspaceName = String

type ReplicationFactor = Int

-- | creates a Cassandra table from the supplied TableDef
createTable :: Consistency -> TableDef -> Cas KeyspaceResult
createTable c td = toEither $ executeSchema c ddl ()
  where
    ddl :: Query Schema () ()
    ddl = fromString ("CREATE TABLE " ++
                      tableName td ++ "(" ++
                      (colString . tablePrimaryKey) td ++ " PRIMARY KEY, " ++
                      L.intercalate ", " ((fmap colString . tableColumns) td) ++
                      ")")
    colString :: Column -> String
    colString (Column n t) = n ++ " " ++ show t

-- | creates a Cassandra keyspace
createKeyspace :: Consistency -> KeyspaceName -> ReplicationFactor -> Cas KeyspaceResult
createKeyspace c n rf = toEither $ executeSchema c ddl ()
  where
    ddl :: Query Schema () ()
    ddl = fromString ("CREATE KEYSPACE " ++ n ++
                          " WITH replication = {\
                          \   'class' : 'SimpleStrategy', \
                          \   'replication_factor' : '" ++ show rf ++ "' \
                          \};")

-- | runs a Keyspace function and converts the function from one that
-- throws an exception to one that returns an Either
toEither :: Cas (Change, Keyspace, Table) -> Cas KeyspaceResult
toEither c = (Right `liftM` c)
             `catch`
             \(e::CassandraException) -> return (Left e)
