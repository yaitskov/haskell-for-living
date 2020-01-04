{-# LANGUAGE TemplateHaskell #-}

module SnowflakePhantom (snowTlake) where


import Language.Haskell.TH
import Data.Hash.MD5
import Data.Tagged

-- snowtlake :: String -> Name -> Q [Dec] ; snowtlake newTypeName baseTypeName = return [TySynD name [] (AppT (AppT (ConT ''Tagged) tag) (ConT baseTypeName)) ] where name = mkName newTypeName ; tag = LitT (StrTyLit (newTypeName ++ "_" ++ nameBase baseTypeName)) ; base = ConT baseTypeName
snowTlake :: String -> Name -> Q [Dec]
snowTlake newTypeName baseTypeName = do
  loc <- location
  let fantom = LitT $ StrTyLit $ newTypeName ++ "_" ++ nameBase baseTypeName ++ md5s (Str $ show loc)
      tag = AppT (ConT ''Tagged) fantom
      base = ConT baseTypeName
      typeDecl = AppT tag base
      name = mkName newTypeName
   in return [TySynD name [] typeDecl]
