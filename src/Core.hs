{-# LANGUAGE OverloadedStrings #-}
module Core
  ( runApplication
  , prepareAppReqs
  , app
  ) where

import           Control.Applicative                (liftA2)
import qualified Control.Exception                  as Ex
import           Control.Monad                      (join)

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod,
                                                     strictRequestBody, responseFile)
import           Network.Wai.Handler.Warp           (run)

import           Network.HTTP.Types                 (status200)
import           Data.Bifunctor                     (first, second, bimap)
import           Data.Either                        (Either (Left, Right),
                                                     either)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Text.IO                       (hPutStrLn)

import qualified Data.ByteString.Lazy.Char8         as LBS

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           System.IO                          (stderr)

import qualified Waargonaut.Encode                  as E

import qualified Conf                       as Conf
import qualified DB                         as DB

import qualified Responses                  as Res
import           Types                      (Conf(..), ConfigError,
                                                     ContentType (PlainText),
                                                     Error (..), RqType (..),
                                                     DBFilePath(..), confPortToWai,
                                                     encodeComment, encodeTopic,
                                                     mkCommentText, mkTopic)

import           AppM                       (App, Env (..), liftEither,
                                                     runApp)

import           Control.Monad.Except               (ExceptT (..), runExceptT)

data StartUpError
  = DBInitErr SQLiteResponse
  | ConfErr ConfigError
  deriving Show

runApplication :: IO ()
runApplication = do
  appE <- runExceptT prepareAppReqs
  either print runWithDBConn appE
  where
    runWithDBConn env =
      appWithDB env >> DB.closeDB (envDB env)

    appWithDB env = Ex.finally
      (run ( confPortToWai . envConfig $ env ) (app env))
      $ DB.closeDB (envDB env)

prepareAppReqs :: ExceptT StartUpError IO Env
prepareAppReqs = ExceptT $ (first ConfErr <$> Conf.parseOptions "files/appconfig.json") >>=
    (\eErrConf -> case eErrConf of
        Left err -> pure $ Left err
        Right conf -> let x = DB.initDB (dbFilePath conf) in
            bimap DBInitErr (Env (liftIO . print) conf) <$> x)

app
  :: Env
  -> Application
app env rq cb =
    runApp (mkRequest rq >>= handleRequest) env >>= cb . handleRespErr
        where
          handleRespErr :: Either Error Response -> Response
          handleRespErr = either mkErrorResponse id

handleRequest
  :: RqType
  -> App Response
handleRequest rqType = case rqType of
  AddRq t c -> Res.resp200 PlainText "Success" <$ DB.addCommentToTopic t c
  ViewRq t  -> Res.resp200Json (E.list encodeComment) <$> DB.getComments t
  ListRq    -> Res.resp200Json (E.list encodeTopic)   <$> DB.getTopics
  StaticRq  -> pure $ responseFile status200 [("Content-Type", "text/html")] "frontend/index.html" Nothing

mkRequest
  :: Request
  -> App RqType
mkRequest rq =
  liftEither =<< case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( ["api", t, "add"], "POST" ) -> liftIO (mkAddRequest t <$> strictRequestBody rq)
    -- View the comments on a given topic
    ( ["api", t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["api", "list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    ("api": t, _)                      -> pure ( Left UnknownRoute )
    -- serve the frontend
    (_, _) -> pure $ Right StaticRq

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute     =
  Res.resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  Res.resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic       =
  Res.resp400 PlainText "Empty Topic"
mkErrorResponse ( DBError _ )    =
  -- Be a sensible developer and don't leak your DB errors over the internet.
  Res.resp500 PlainText "OH NOES"
