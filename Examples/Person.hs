{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module Examples.Person
       (
         Person(..),
         peopleTableDef
       )
where

import Data.Maybe (listToMaybe)
import Data.UUID (UUID(..))
import Database.Cassandra.CQL
import Data.Text (Text)
import Data.String (fromString)
import Database.Cassandra.CqlHelper

data Person = Person {
  personId :: UUID,
  firstName :: Text,
  lastName :: Text
  } deriving (Show)

peopleTableDef = TableDef "people" [
  Column "firstName" CText,
  Column "lastName" CText
  ] (Column "id" CUuid)

instance Row Person where
  type Tuple Person = (UUID, Text, Text)
  toTuple p = (personId p, firstName p, lastName p)
  fromTuple (id,fname,lname) = Person id fname lname

instance Readable Person where
  findAll c mlim = do
    let lim = case mlim of
          Just n  -> " limit " ++ show n
          Nothing -> ""
    let query = fromString ("select id, firstName, lastName from people" ++ lim)
    res <- executeRows c query ()
    return $ map fromTuple res

  findById c id = do
    let query = fromString ("select id, firstName, lastName from people where id = " ++ show id)
    res <- executeRows c query ()
    return $ fmap fromTuple (listToMaybe res)

instance Writable Person where
  insert c p = do
    let query = "insert into people (id, firstName, lastName) values (?, ?, ?)"
    executeWrite c query (toTuple p)

instance Destroyable Person where
  delete c r = executeWrite c query ()
    where
      query = fromString ("delete from people where id = " ++ show (personId r))
