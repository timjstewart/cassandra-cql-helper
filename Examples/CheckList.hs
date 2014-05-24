
module Examples.CheckList
       (
         checklistTableDef
       )
       where

import Database.Cassandra.CQL
import Database.Cassandra.CqlHelper

checklistTableDef = TableDef "checklist" [
  Column "checklistName" CText,
  Column "author" CUuid
  ] (Column "id" CUuid)

