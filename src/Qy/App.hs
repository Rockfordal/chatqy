{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Qy.App where

import           Control.Monad.Reader
import           Network.Wai
import           Servant

import           Qy.Config                              (Config)
import           Qy.Page                                (Eg, egServer)
import           Qy.Room                                (RoomAPI, roomServer)
import           Qy.Types
import           Qy.User                                (Token, UserAPI,
                                                         loginServer)

import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger   (logStdoutDev)
import           Network.Wai.Middleware.Servant.Options (provideOptions)

type API = "api" :> QyAPI
      :<|> "static" :> Raw
      :<|> Eg

type QyAPI = UserAPI :<|> LoginRequiredAPI

type LoginRequiredAPI = Header "Authorization" Token :> RoomAPI

server :: ServerT QyAPI AppM
server = loginServer
    :<|> loginRequiredServer

loginRequiredServer :: ServerT LoginRequiredAPI AppM
loginRequiredServer t = roomServer t

readerServer :: Config -> Server API
readerServer cfg = enter (readerToEither cfg) server
                   :<|> serveDirectory "build"
                   :<|> egServer

readerToEither :: Config -> AppM :~> Handler
readerToEither cfg = Nat $ \x -> runReaderT x cfg

api :: Proxy API
api = Proxy

-- app :: Config -> Application
-- app cfg = serve api (readerServer cfg)

-- app :: Config -> Application
-- app cfg = simpleCors $ serve api (readerServer cfg)

-- app :: Config -> Application
-- app cfg =
--     -- logStdoutDev
--     -- $ cors (const $ Just policy)
--     provideOptions api
--     $ serve api (readerServer cfg)
--   where
--   policy = simpleCorsResourcePolicy
--            { corsRequestHeaders = [ "content-type" ] }

app :: Config -> Application
app cfg =
    -- do
    -- s <- getServer
    --   s <- (readerServer cfg)
    --   return
        logStdoutDev
        -- $ provideOptions api
        $ corsWithContentType
        $ serve api (readerServer cfg)
    where
        corsWithContentType :: Middleware
        corsWithContentType = cors (const $ Just policy)
            where
              policy = simpleCorsResourcePolicy
                { corsRequestHeaders = ["Content-Type"] }
