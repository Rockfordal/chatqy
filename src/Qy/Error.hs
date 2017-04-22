module Qy.Error where

import           Control.Monad.Reader
import           Servant

import           Qy.Types             (AppM)


class ToServantErr a where
    toServantErr :: a -> ServantErr

instance ToServantErr ServantErr where
    toServantErr = id

raiseHTTPError :: (ToServantErr e) => e -> AppM a
raiseHTTPError = lift . throwError . toServantErr
