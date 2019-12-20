{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Text.RawString.QQ
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Concurrent.Map as CCM
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Network.HTTP.Media ((//), (/:))
import Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString.Char8 as BS

-- data User = User
--   { userId        :: Int
--   , userFirstName :: String
--   , userLastName  :: String
--   } deriving (Eq, Show)


-- data MainState {
--   m :: CCM.Map String String
-- }

-- type AppM = ReaderT MainState Handler

-- $(deriveJSON defaultOptions ''User)

-- type API = "users" :> (Get '[JSON] [User]) --  :<|> Post '[JSON] NoContent)

data Html

instance Accept Html where
  contentType _ = BS.pack "text" // BS.pack "html"

instance MimeRender Html String where
  mimeRender _ val = BSL.pack val

type API = "static" :> Raw :<|> "hello" :> Get '[PlainText] String :<|> Get '[Html] String

api :: Proxy API
api = Proxy

-- server :: Server API
-- server = returnUsers
staticServer = serveDirectoryFileServer "."

hello = return "Hello world from Servant"

myIndex = return [r|<!DOCTYPE html>
<html>
<head>
  <title>index page</title>
</head>
<body>
  <h1>Well on Servant demo site!</h1>
  <ul>
    <li><a href="/hello">Hello</a></li>
    <li><a href="/static">Static files</a></li>
  </ul>
</body>
</html>
|]

app :: Application
app = serve api (staticServer :<|> hello :<|> myIndex)

startApp :: IO ()
startApp = run 9081 app

--storePair key value =
-- returnUsers = return users

-- users :: [User]
-- users = [ User 1 "Isaac" "Newton"
--         , User 2 "Albert" "Einstein"
--         ]
