{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( app
  )
where

import           Data.ByteString.Lazy.UTF8      ( ByteString )
import           Network.HTTP.Types             ( notFound404 )
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import qualified Network.Wai as Wai
import           Servant

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ = id

type API
  =     Get '[HTML] ByteString
  :<|>  Raw

server :: Server API
server
  =     return "<h1>Hellow, World.</h1>"
  :<|>  return raw404
  where
    raw404 = res notFound404 "404 Not Found."
    res status body _ respond = respond $ Wai.responseLBS status [("Content-Type", "text/html;charset=utf-8")] body

app :: Application
app = serve (Proxy @API) server
