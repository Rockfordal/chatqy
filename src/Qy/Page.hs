{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Qy.Page where

import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A


page :: Html
page = docTypeHtml $ do
    H.head $ do
        H.title  "ChatQy"
        H.meta ! charset "UTF-8"
        H.link ! rel "stylesheet" ! href "/static/app.css"
    H.body $ do
        H.script ! src "/static/bundle.js" $ ""

type Eg = Get '[HTML] Html
     :<|> "login" :> Get '[HTML] Html
     :<|> "signup" :> Get '[HTML] Html


egServer :: Server Eg
egServer = return page
      :<|> return page
      :<|> return page
