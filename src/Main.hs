module Main where

import           Control.Applicative         ((<$>))
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Yaml.Config            (load, lookup, lookupDefault,
                                              subconfig)
import           Database.Persist.Postgresql (runSqlPool)
import           Network.Wai.Handler.Warp    (run)
import           Prelude                     hiding (lookup)
import           System.Environment          ()

import           Qy.App                      (app)
import           Qy.Config
import           Qy.Model                    (doMigrations)

import           Qy.Chat.Simple              (appWithSocket)

main :: IO ()
main = do
    config <- load "./chatqy.yaml"

    qyConfig <- subconfig "chatqy" config

    let port = lookupDefault "port" 4000 qyConfig
        poolNum = lookupDefault "poolNum" 1 qyConfig
    connStr <- encodeUtf8 <$> lookup "connStr" qyConfig
    env <- lookup "env" qyConfig

    pool <- makePool env connStr poolNum
    rmap <- makeTokenMap
    tmap <- makeRoomMap

    let cfg = defaultConfig { getPool = pool
                            , getEnv = env
                            , getTokenMap = rmap
                            , getRoomMap = tmap}
        logger = setLogger env
    runSqlPool doMigrations pool
    run port . logger . appWithSocket cfg $ app cfg
