{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Cassandra.CQL
import Control.Monad.Trans (liftIO)

import Database.Cassandra.CqlHelper
import Examples.Person
import Examples.CheckList

servers = [("localhost", "9042")]

initialize :: IO ()
initialize = do
  pool <- newSystemPool servers
  runCas pool $ createKeyspace QUORUM "checklist" 1
  return ()

main = do
  initialize
  
  appPool <- newPool servers "checklist"
  runCas appPool $ do
    createTable QUORUM peopleTableDef
    createTable QUORUM checklistTableDef

    id <- genId
    insert QUORUM (Person id "Tim" "Stewart")

    people <- findAll QUORUM (Just 2) :: Cas [Person]
    liftIO $ print people

    found <- findById QUORUM id :: Cas (Maybe Person)
    liftIO $ print found

    case found of
      Just r -> delete QUORUM r
      Nothing -> return ()
