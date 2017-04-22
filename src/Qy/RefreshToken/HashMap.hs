module Qy.RefreshToken.HashMap (
    RTokenMap
    , newRTokenMap
    , addToken'
    , existsToken'
    , deleteToken'
    ) where

import           Control.Concurrent.STM
import           Data.HashMap.Strict    as M
import           Data.Text

type RTokenMap = TVar (HashMap Text Text)

newRTokenMap :: IO RTokenMap
newRTokenMap = atomically $ newTVar M.empty

addToken' :: RTokenMap -> Text -> Text -> IO ()
addToken' rm uid token = atomically $ do
    m <- readTVar rm
    writeTVar rm $ insert uid token m

existsToken' :: RTokenMap -> Text -> Text -> IO Bool
existsToken' rm uid token = do
    m <- readTVarIO rm
    return $ maybe False (== token) (M.lookup uid m)

deleteToken' :: RTokenMap -> Text -> IO ()
deleteToken' rm uid = atomically $ do
    m <- readTVar rm
    writeTVar rm $ delete uid m
