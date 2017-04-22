module Qy.Types where

import           Control.Monad.Reader
import           Data.Text            (Text)
import           Servant

import           Qy.Config

type AppM = ReaderT Config Handler

type UserName = Text

toAppM :: Handler a -> AppM a
toAppM = lift
