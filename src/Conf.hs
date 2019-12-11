{-# LANGUAGE OverloadedStrings #-}
module Conf
    ( Conf (..)
    , firstAppConfig
    ) where

data Conf = Conf
  { dbFilePath :: FilePath
  }

firstAppConfig :: Conf
firstAppConfig = Conf "app_db.db"
