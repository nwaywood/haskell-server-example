{-# LANGUAGE OverloadedStrings #-}
module DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql
import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           DB.Types                   (DBComment)
import           AppM                       (AppM(..), runAppM)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB aToEErrB ioA =
  -- This function is intended to abstract away the running of DB functions and
  -- the catching of any errors. As well as the process of running some
  -- processing function over those results.
  AppM $ Sql.runDBAction ioA >>= (\eErrA -> case eErrA of
      Left err -> pure $ Left (DBError err)
      Right val -> pure $ aToEErrB val)
  -- Move your use of DB.runDBAction to this function to avoid repeating
  -- yourself in the various DB functions.

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments firstAppDB topic =
    let
      sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
      getDBComments :: IO [DBComment]
      getDBComments = Sql.query (dbConn firstAppDB) sql (Sql.Only $ getTopic topic)
      in
         runDB (traverse fromDBComment) getDBComments

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic db topic comment =
    let
      sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
      addComment t = Sql.execute (dbConn db) sql (getTopic topic, getCommentText comment, t)
    in
      AppM $ getCurrentTime >>= (\time ->
          runAppM $ runDB pure (addComment time))

instance FromRow Text where
  fromRow = field

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics db =
      let
        sql = "SELECT DISTINCT topic FROM comments"
        getTopics_ :: IO [Text]
        getTopics_ = Sql.query_ (dbConn db) sql
      in
        runDB (traverse mkTopic) getTopics_

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic db topic =
      let
        sql = "DELETE FROM comments WHERE topic = ?"
        deleteTopic_ :: IO ()
        deleteTopic_ = Sql.execute (dbConn db) sql (Sql.Only $ getTopic topic)
      in
        runDB pure deleteTopic_

-- Go to 'src/Level05/Core.hs' next.
