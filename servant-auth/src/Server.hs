{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( mainWithCookies
  )
where

import           Data.ByteString.Lazy.UTF8      ( ByteString )
import           Network.HTTP.Types             ( notFound404 )
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           GHC.Generics                   ( Generic )
import qualified Network.Wai                   as Wai
import           Network.Wai.Handler.Warp       ( run )
import           Servant
import           Servant.Auth.Server
import           Web.FormUrlEncoded             ( FromForm(..)
                                                , parseUnique
                                                , parseMaybe
                                                )

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ = id

data User = User { name :: String, email :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

data Signin = Signin { name :: String, email :: String }
   deriving (Eq, Show, Read, Generic)

instance FromForm Signin where
  fromForm f = Signin
    <$> parseUnique "name"  f
    <*> parseUnique "email" f

data Login = Login { name :: String, token :: String }
   deriving (Eq, Show, Read, Generic)

instance FromForm Login where
  fromForm f = Login
    <$> parseUnique "name"  f
    <*> parseUnique "token" f

type Protected
  =     "name"  :> Get '[JSON] String
  :<|>  "email" :> Get '[JSON] String

protected :: AuthResult User -> Server Protected
protected (Authenticated user)
  =     return (name (user :: User))
  :<|>  return (email (user :: User))
protected _ = throwAll err401

type Unprotected
  =     Get '[HTML] ByteString
  :<|>  "signin" :> Get '[HTML] ByteString
  :<|>  "signin" :> ReqBody '[FormUrlEncoded] Signin
                 :> Verb 'POST 308 '[PlainText]
                                   (Headers '[ Header "Location" String
                                             , Header "Set-Cookie" SetCookie
                                             , Header "Set-Cookie" SetCookie]
                                            NoContent)
  :<|>  "login" :> Get '[HTML] ByteString
  :<|>  "login" :> ReqBody '[FormUrlEncoded] Login
                :> Verb 'POST 308 '[PlainText]
                                  (Headers '[ Header "Location" String
                                            , Header "Set-Cookie" SetCookie
                                            , Header "Set-Cookie" SetCookie]
                                           NoContent)
  :<|>  Raw

unprotected :: CookieSettings -> JWTSettings -> Server Unprotected
unprotected cs jwt
  =     return "<h1>Hellow, World.</h1>"
  :<|>  return ""
  :<|>  signin cs jwt
  :<|>  return ""
  :<|>  login cs jwt
  :<|>  return raw404
  where
    raw404 = res notFound404 "404 Not Found."
    res status body _ respond = respond $ Wai.responseLBS status [("Content-Type", "text/html;charset=utf-8")] body

type API
  =     (Auth '[Cookie] User :> Protected)
  :<|>  Unprotected

server :: CookieSettings -> JWTSettings -> Server API
server cs jwt
  =     protected
  :<|>  unprotected cs jwt

mainWithCookies :: IO ()
mainWithCookies = do
  key <- generateKey
  let ckiCfg = defaultCookieSettings { sessionCookieName = "sid", cookieXsrfSetting = Nothing }
      jwtCfg = defaultJWTSettings key
      ctx = ckiCfg :. jwtCfg :. EmptyContext
  run 80 $ serveWithContext (Proxy @API) ctx (server ckiCfg jwtCfg)

signin :: CookieSettings -> JWTSettings -> Signin -> Handler (Headers '[ Header "Location" String
                                                                       , Header "Set-Cookie" SetCookie
                                                                       , Header "Set-Cookie" SetCookie]
                                                                      NoContent)
signin cookieSettings jwtSettings Signin { name, email } = do
   mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings User { name, email }
   case mApplyCookies of
     Nothing           -> throwError err401
     Just applyCookies -> return (addHeader "" $ applyCookies NoContent :: Headers '[Header "Location" String, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

login :: CookieSettings -> JWTSettings -> Login -> Handler (Headers '[ Header "Location" String
                                                                     , Header "Set-Cookie" SetCookie
                                                                     , Header "Set-Cookie" SetCookie]
                                                                    NoContent)
login cookieSettings jwtSettings Login { name, token } = do
   mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings User { name, email = "" }
   case mApplyCookies of
     Nothing           -> throwError err401
     Just applyCookies -> return (addHeader "" $ applyCookies NoContent :: Headers '[Header "Location" String, Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
