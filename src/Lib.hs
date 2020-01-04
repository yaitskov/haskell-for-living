{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Lib
    ( startApp
    , app
    ) where

import qualified Language.C.Inline as C

-- import Data.Tagged
import Data.Aeson
import Data.Aeson.TH
import Text.RawString.QQ
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import qualified Data.List as L
import qualified Data.Text as T
import System.Exit
import Control.Exception (try, throw, displayException)
--  import Control.Monad.Catch
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Map as CCM
-- import Control.Concurrent.STM (readTVarIO, newTVarIO, atomically)
import Data.IORef (readIORef, writeIORef, newIORef, IORef)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT, runReader)
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Network.HTTP.Media ((//), (/:))
import Data.ByteString.Lazy.Char8 as BSL
import Data.ByteString.Char8 as BS
import Data.CaseInsensitive as CI
import Control.Monad (forM_)
import Servant.HTML.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import GHC.Generics
import Web.FormUrlEncoded (FromForm)

C.include "<stdio.h>"
C.include "<stdlib.h>"

type RedirectDest = String

data RedirectEntry = RedirectEntry
  {
    destination :: RedirectDest
  } deriving (Eq, Ord, Show)


type RedirectKey = String

data MyAppState = MyAppState {
  m :: CCM.Map RedirectKey RedirectEntry,
  serverShutdown  :: IORef (IO ()),
  luckyNumber :: Integer
}

type AddRedirectMappingPage = H.Html

data RedirectMappingForm = RedirectMappingForm
  { key :: !T.Text,
    destination :: !T.Text
  } deriving (Show, Eq, Generic)

instance FromForm RedirectMappingForm

data Html

instance Accept Html where
  contentType _ = BS.pack "text" // BS.pack "html"

instance MimeRender Html String where
  mimeRender _ val = BSL.pack val

instance {-# OVERLAPS #-} (Show a) => MimeRender PlainText a where
  mimeRender _ val = BSL.pack $ show val

type MyAppMo = ReaderT MyAppState IO
type PageWithListOfRedirect = H.Html -- Tagged "page-with-redirects" H.Html

type RePar = QueryParam' '[Required, Strict]

type FuneralApi = ("die-slowly" :> Post '[PlainText] String
                   :<|> "die-fast" :> Post '[PlainText] ())

type BusinessLogicApi = (("add-mapping" :> (RePar "key" RedirectKey
                                            :> RePar "destination" RedirectDest
                                            :> Post '[PlainText] Bool
                                            :<|> Get '[HTML] AddRedirectMappingPage
                                            :<|> "via-form" :> ReqBody '[FormUrlEncoded] RedirectMappingForm :> Post '[PlainText] ()))
                        :<|> "no-way" :> RePar "path" String :> Get '[PlainText] String
                        :<|> "redirects-sorted-by-key" :> Get '[HTML] PageWithListOfRedirect
                        :<|> Capture "key" RedirectKey :> Get '[PlainText] ())

type MiscApi = ("static" :> Raw
                :<|> "hello" :> Get '[PlainText] String
                :<|> Get '[Html] String)

type API = (FuneralApi :<|> MiscApi :<|> BusinessLogicApi)

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
    Just entry -> redirectTo $ destination (entry :: RedirectEntry)

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
    <li><a href="/redirects-sorted-by-key">all redirects sorted by key</a></li>
    <li><a href="/add-mapping">Add redirect mapping</a></li>
    <li><form method="POST" action="/die-fast"><button type="submit">Die Fast</button></form></li>
    <li><form method="POST" action="/die-slowly"><button type="submit">Die Slowly</button></form></li>
    <li><a href="/static/">Static files</a></li>
  </ul>
</body>
</html>
|]


cExitNormal :: IO ()
cExitNormal = do
  x <- [C.exp| void{ exit(0) } |]
  Prelude.putStrLn $ show x

dieFast = do
  liftIO $ Prelude.putStrLn "By..." >> cExitNormal

dieSlowly = do
  st <- ask
  liftIO $ Prelude.putStrLn "Bye-bye..." >> readIORef (serverShutdown st) >>= \cb -> cb
  return "You was able to get my response but I am already dead. Bye-bye..."

myIndex = do
  n <- ask
  return $ indexHead ++ (show . luckyNumber $ n) ++ indexTail

initState = do
  m <- CCM.empty
  CCM.insert "default" RedirectEntry { destination = "http://ya.ru" } m
  CCM.insert "sam" RedirectEntry { destination = "http://cia.gov" } m
  shutdownCbBox <- liftIO $ newIORef (Prelude.putStrLn "Server shutdown callback is not set")
  return  MyAppState { m = m,
                       luckyNumber = 877,
                       serverShutdown = shutdownCbBox }

trans :: MyAppState -> ReaderT MyAppState IO x -> Handler x
trans st rdr = Handler $ liftIO (try (runReaderT rdr st)) >>=
  \ei -> case ei of
    Left e -> liftIO (Prelude.putStrLn $ "My Exception " ++ displayException e) >> throwError e
    Right ok -> return ok


showRedirectsSortedByKey :: MyAppMo PageWithListOfRedirect
showRedirectsSortedByKey = do
  st <- ask
  sortedPairs <- liftIO $ fmap L.sort $ CCM.unsafeToList $ m st
  liftIO $ Prelude.putStrLn $ " pairs  " ++ show sortedPairs
  return $ H.docTypeHtml $ do
    H.head $ do
      H.title "Redirects by key"
    H.body $ do
      H.h1 "Redirects by key"
      forM_ [1,2,3] (\x -> return $ (H.p . H.toHtml . show) x) :: H.Html
      --      H.table $ forM_ sortedPairs (\(key,dest) -> do
      H.table $ do
        forM_ sortedPairs (\(key,dest) -> do
                              return $ H.tr $ do
                                H.td $ H.toHtml key
                                H.td $ H.toHtml $ destination (dest :: RedirectEntry))


addRedirectMappingPage :: ReaderT MyAppState IO AddRedirectMappingPage
addRedirectMappingPage = return $ H.docTypeHtml $ do
  H.head $ do
    H.title "New redirect"
  H.body $ do
    H.h1 "New redirect"
    H.div $ do
      H.form H.! A.method "POST" H.! A.action "/add-mapping/via-form" $ do
        H.div $ do
          H.label $ do
            H.span $ "Key"
            H.input H.! A.name "key" H.! A.type_ "text"
          H.p "Enter key which is path relative to root of url"
        H.div $ do
          H.label $ do
            H.span $ "Destiation URL"
            H.input H.! A.name "destination"
            H.p "Then enter full destination URL with http:// or https://"
        H.div $ do
          H.button H.! A.type_ "submit" $ do
            H.span $ "add"


addRedirectMappingFromForm :: RedirectMappingForm -> ReaderT MyAppState IO ()
addRedirectMappingFromForm form = do
  addRedirectMapping (T.unpack $ key form) (T.unpack $ destination (form :: RedirectMappingForm))
  redirectTo "/"

addRedirectMapping :: RedirectKey -> RedirectDest -> ReaderT MyAppState IO Bool
addRedirectMapping key dest = do
  st <- ask
  liftIO $ CCM.insert key RedirectEntry { destination = dest } $ m st


funeral = dieSlowly :<|> dieFast
businessLogic = (addRedirectMapping :<|> addRedirectMappingPage :<|> addRedirectMappingFromForm) :<|> noWay :<|> showRedirectsSortedByKey :<|> resolveAndRedirect
misc = staticServer :<|> hello :<|> myIndex

app :: MyAppState -> Application
app appSt = serve api (hoistServer api
                       (\x -> trans appSt x)
                       (funeral :<|> misc :<|> businessLogic))

startApp :: IO ()
startApp = do
  st <- initState
  runSettings (setTimeout 3
                (setInstallShutdownHandler (\closeSocket -> Prelude.putStrLn "Binding callback for closing socket..." >>
                                             writeIORef (serverShutdown st) closeSocket)
                (setServerName (BS.pack "DieHard")
                 (setPort 9081
                  (setOnOpen (\addr -> Prelude.putStrLn "New conneciton" >> return True)
                   (setBeforeMainLoop
                     (Prelude.putStrLn "servant is ready to serve")
                     defaultSettings))))))
    (app st)
