{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Qy.RefreshToken (
    addToken
    , existsToken
    , deleteToken
    ) where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader
import           Data.Text.Internal      (Text)

import           Qy.Config
import           Qy.RefreshToken.HashMap

addToken :: (MonadIO m, MonadReader Config m) => Data.Text.Internal.Text -> Data.Text.Internal.Text -> m ()
addToken uid token = do
    rmap <- asks getTokenMap
    liftIO $ addToken' rmap uid token

existsToken :: (MonadIO m, MonadReader Config m) => Data.Text.Internal.Text -> Data.Text.Internal.Text -> m Bool
existsToken uid token = do
    rmap <- asks getTokenMap
    liftIO $ existsToken' rmap uid token

deleteToken :: (MonadIO m, MonadReader Config m) => Data.Text.Internal.Text -> m ()
deleteToken uid = do
    rmap <- asks getTokenMap
    liftIO $ deleteToken' rmap uid
