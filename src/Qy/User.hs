{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Qy.User where

import qualified Control.Applicative    as A
import           Control.Monad          (when)
import qualified Crypto.Hash.SHA256     as H
import           Data.Aeson
import           Data.Text
import           Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)
import           GHC.Generics
import           Servant

import           Control.Monad.IO.Class (liftIO)

import           Web.JWT                hiding (JSON)
import qualified Web.JWT                as J

import           Data.Time.Clock        (NominalDiffTime)
import           Data.Time.Clock.POSIX  (getPOSIXTime)

import           Database.Persist

import           Qy.Error               (raiseHTTPError)
import           Qy.Error.Auth          (passwordInvalid, passwordTooShort,
                                         refreshTokenInvalid, tokenExpire,
                                         tokenInvalid, tokenNotPresent,
                                         userAlreadyExists, userNameIsEmpty,
                                         userNotExists)
import qualified Qy.Model               as M
import           Qy.Random              (randomText)
import           Qy.RefreshToken        (addToken, deleteToken, existsToken)
import           Qy.Types


type UserAPI = "login" :> ReqBody '[JSON] LoginForm :> Post '[JSON] ReturnToken
          :<|> "signup" :> ReqBody '[JSON] SignUpForm :> Post '[JSON] ReturnToken
          :<|> "refresh_token" :> ReqBody '[JSON] RefreshForm :> Post '[JSON] ReturnToken
          :<|> "logout" :> Header "Authorization" Token :> Get '[JSON] Text

data LoginForm = LoginForm { username :: Text
                           , password :: Text
                           } deriving (Show, Generic)
instance FromJSON LoginForm

data SignUpForm = SignUpForm { signname  :: Text
                             , signpass  :: Text
                             , signemail :: Text
                             } deriving (Show, Generic)
instance FromJSON SignUpForm where
    parseJSON (Object v) = SignUpForm
              <$> v .: "username"
              <*> v .: "password"
              <*> v .: "email"
    parseJSON _ = A.empty

data RefreshForm = RefreshForm { uid   :: Text
                               , token :: Text } deriving Generic
instance FromJSON RefreshForm


data ReturnToken = ReturnToken { access_token  :: Text
                               , refresh_token :: Text
                               , expiration    :: Int --seconds to expire
                               } deriving Generic

instance ToJSON ReturnToken

newtype Token = Token Text

instance FromHttpApiData Token where
    parseQueryParam t =
        let striped = T.strip t
            ls = T.words striped
        in case ls of
                "Bearer":r:_ -> Right $ Token r
                _            -> Left "Invalid Token"


loginServer :: ServerT UserAPI AppM
loginServer = login :<|> signup :<|> refresh :<|> logout

login :: LoginForm -> AppM ReturnToken
login (LoginForm u p) = do
    -- allow all to login
    maybeUser <- M.runDb $ getBy $ M.UniqueUser u
    case maybeUser of
      Nothing              -> raiseHTTPError userNotExists
      Just (Entity _ user) -> returnTokenWithValidation user p

signup :: SignUpForm -> AppM ReturnToken
signup (SignUpForm u p e) = do
    when (T.null u) $ raiseHTTPError userNameIsEmpty
    when (T.length p < 6) $ raiseHTTPError passwordTooShort
    let sha_p = H.hash $ encodeUtf8 p
        user = M.User u sha_p e
    dbResult <- M.runDb $ insertBy user
    case dbResult of
      Left _ -> raiseHTTPError userAlreadyExists
      _      -> returnToken u

refresh :: RefreshForm -> AppM ReturnToken
refresh (RefreshForm uid token) = do
    exists <- existsToken uid token
    if exists
    then returnToken uid
    else raiseHTTPError refreshTokenInvalid

logout :: Maybe Token -> AppM Text
logout t = do
    uid <- checkToken t
    deleteToken uid
    return "SUCCESSED"


returnTokenWithValidation :: M.User -> Text -> AppM ReturnToken
returnTokenWithValidation (M.User u p _) password
    | p /= enc_password = raiseHTTPError passwordInvalid
    | otherwise = returnToken u
  where enc_password = H.hash $ encodeUtf8 password

returnToken :: Text -> AppM ReturnToken
returnToken uid = do
    expTime <- liftIO $ createExpTime 60 -- expire at 1 hour
    let cs = def { iss = stringOrURI uid
                 , J.exp = numericDate expTime
                 }
        s = getSecret
        alg = getAlgorithm
        token = encodeSigned alg s cs
    randomToken <- liftIO $ randomText
    addToken uid randomToken
    return $ ReturnToken token randomToken (60 * 60)


getSecret :: Secret
getSecret = secret "wwaaifidsa9109f0dasfda-=2-13"

getAlgorithm :: Algorithm
getAlgorithm = HS256


-- some function for doing token check

createExpTime :: Int -> IO NominalDiffTime
createExpTime minn = do
    cur <- getPOSIXTime
    return $ cur + (fromIntegral minn + 5) * 60 -- add 5 more minutes

checkExpValid :: JWTClaimsSet -> IO Bool
checkExpValid = checkExpValid' . J.exp

checkExpValid' :: Maybe NumericDate -> IO Bool
checkExpValid' Nothing = return False
checkExpValid' (Just d) = do
    cur <- getPOSIXTime
    return (secondsSinceEpoch d > cur)

getClaimSetFromToken :: Token -> Maybe JWTClaimsSet
getClaimSetFromToken (Token t) =
    fmap claims $ decodeAndVerifySignature getSecret t

checkToken :: Maybe Token -> AppM Text
checkToken Nothing = raiseHTTPError tokenNotPresent
checkToken (Just t) = do
    case getClaimSetFromToken t of
      Nothing -> raiseHTTPError tokenInvalid
      Just cl -> do
          isValid <- liftIO $ checkExpValid cl
          if isValid
          then case iss cl of
                 Nothing -> raiseHTTPError tokenInvalid
                 Just u  -> return $ J.stringOrURIToText u
          else raiseHTTPError tokenExpire
