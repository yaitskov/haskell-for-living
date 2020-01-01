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
import Control.Exception (try, throw)
--  import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Map as CCM
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT, runReader)
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Network.HTTP.Media ((//), (/:))
import Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString.Char8 as BS
import Data.CaseInsensitive as CI
type RedirectDest = String

data RedirectEntry = RedirectEntry
  {
    destination :: RedirectDest
  } deriving (Eq, Show)


type RedirectKey = String

data MyAppState = MyAppState {
  m :: CCM.Map RedirectKey RedirectEntry,
  luckyNumber :: Integer
}

data Html

instance Accept Html where
  contentType _ = BS.pack "text" // BS.pack "html"

instance MimeRender Html String where
  mimeRender _ val = BSL.pack val

instance MimeRender PlainText () where
  mimeRender _ _ = BSL.pack "()"

type API = "static" :> Raw :<|> "hello" :> Get '[PlainText] String :<|> Get '[Html] String :<|> "no-way" :> QueryParam' '[Required, Strict] "path" String :> Get '[PlainText] String :<|> Capture "key" RedirectKey :> Get '[PlainText] ()

api :: Proxy API
api = Proxy

redirectTo :: String -> ReaderT MyAppState IO ()
redirectTo destUrl = throw $ err301 {errHeaders = [(CI.mk $ BS.pack "Location", BS.pack destUrl)]}

resolveAndRedirect :: String -> ReaderT MyAppState IO ()
resolveAndRedirect key = do
  st <- ask
  destO <- liftIO $ CCM.lookup key $ m st
  case destO of
    Nothing -> redirectTo $ "/no-way?path=" ++ key
    Just entry -> redirectTo $ destination entry

staticServer = serveDirectoryFileServer "."

hello :: ReaderT MyAppState IO String
hello = return "Hello world from Servant"

noWay :: String -> ReaderT MyAppState IO String
noWay path = return $ "Key " ++ path ++ " is not bound to any destination."

indexHead :: String
indexHead = [r|<!DOCTYPE html>
<html>
<head>
  <title>index page</title>
</head>
<body>
  <h1>Welcome on Servant demo site!</h1>
  <p> n = |]

indexTail :: String
indexTail = [r|
  <ul>
    <li><a href="/hello">Hello</a></li>
    <li><a href="/static/">Static files</a></li>
  </ul>
</body>
</html>
|]

myIndex = do
  n <- ask
  return $ indexHead ++ (show . luckyNumber $ n) ++ indexTail

initState = do
  m <- CCM.empty
  CCM.insert "default" RedirectEntry { destination = "http://ya.ru" } m
  CCM.insert "sam" RedirectEntry { destination = "http://cia.gov" } m
  return  MyAppState { m = m, luckyNumber = 877 }

-- instance Exception

trans :: MyAppState -> ReaderT MyAppState IO x -> Handler x
trans st rdr = Handler $ liftIO (try (runReaderT rdr st)) >>=
  \ei -> case ei of
    Left e -> throwError e
    Right ok -> return ok


app :: MyAppState -> Application
app appSt = serve api (hoistServer api
                       (\x -> trans appSt x)
                       (staticServer :<|> hello :<|> myIndex :<|> noWay :<|> resolveAndRedirect))
-- app = serve api (hoistServer api (\x -> return (runReader x 777)) (staticServer :<|> hello :<|> myIndex))

-- app = serveWithContext api (777 :. EmptyContext)  (hoistServerWithContext api Proxy :: Proxy '[] (\x -> return (runReader x  (staticServer :<|> hello :<|> myIndex)

startApp :: IO ()
startApp = do
  st <- initState
  run 9081 (app st)

--storePair key value =
-- returnUsers = return users

-- users :: [User]
-- users = [ User 1 "Isaac" "Newton"
--         , User 2 "Albert" "Einstein"
--         ]
