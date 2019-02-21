{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( app
  )
where

import           Prelude                        ( id
                                                , return
                                                )
import           Data.ByteString.Lazy.UTF8      ( ByteString )
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Servant

data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML ByteString where
  mimeRender _ = id

type RootAPI = Get '[HTML] ByteString

server :: Server RootAPI
server = return "<h1>Hellow, World.</h1>"

app :: Application
app = serve (Proxy @RootAPI) server
