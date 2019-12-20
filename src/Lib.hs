{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Concurrent.Map as CCM
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Servant.Server.StaticFiles (serveDirectoryFileServer)
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

type API = "static" :> Raw

api :: Proxy API
api = Proxy

-- server :: Server API
-- server = returnUsers
server = serveDirectoryFileServer "."

app :: Application
app = serve api server

startApp :: IO ()
startApp = run 9081 app

--storePair key value =
-- returnUsers = return users

-- users :: [User]
-- users = [ User 1 "Isaac" "Newton"
--         , User 2 "Albert" "Einstein"
--         ]
