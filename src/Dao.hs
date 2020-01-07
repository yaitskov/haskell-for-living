{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dao (
  RedirectMappingR(..),
  Key(..),
  openDbPool
  ) where

import Control.Monad.Logger (NoLoggingT(..))
import Data.Pool (Pool)
-- import Database.Persist (SqlBackend)
import Database.Persist.Class (Key(..))
import Database.Persist.Sql (runSqlPool, SqlBackend)
import Database.Persist.MySQL (createMySQLPool, mkMySQLConnectInfo)
import Database.Persist.TH (persistLowerCase, mkPersist, sqlSettings)


mkPersist sqlSettings ([persistLowerCase|
RedirectMappingR sql=redirect_mapping
  anchor String sql=matching_key maxlen=20
  value String sql=destination maxlen=2000
  Primary anchor
  UniqueAnchor anchor
  deriving Show Read Eq Ord
|])


openDbPool = runNoLoggingT (createMySQLPool cinf 2 :: NoLoggingT IO (Pool SqlBackend))
  where
    cinf = mkMySQLConnectInfo "localhost" "root" "root" "redirect_mapping_dev"
