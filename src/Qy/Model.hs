{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Qy.Model where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import           Data.ByteString        (ByteString)
import           Data.Text
import           Database.Esqueleto
import           Database.Persist.TH
import           Prelude                as P

import           Qy.Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    password ByteString
    email Text
    UniqueUser name
    deriving Show
Room
    name Text
    description Text Maybe
    createdBy UserId
    UniqueRoom name
    deriving Show
UserRoom
    userId UserId
    roomId RoomId
    UniqueUserRoom userId roomId
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadIO m, MonadReader Config m) => ReaderT SqlBackend IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

checkUserInRoom :: (MonadReader Config m, MonadIO m) => Text -> Text -> m Bool
checkUserInRoom uname rname = do
    _pool <- asks getPool
    rooms <- runDb $ select $
      from $ \(u, re, r) -> do
        where_ (u^.UserId ==. re^.UserRoomUserId
               &&. re^.UserRoomRoomId ==. r^.RoomId
               &&. u^.UserName ==. val uname
               &&. r^.RoomName ==. val rname)
        return r
    return (not $ P.null rooms)
