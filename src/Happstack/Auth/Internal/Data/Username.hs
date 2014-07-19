{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Internal.Data.Username where

import Data.Data
import Data.SafeCopy

newtype Username = Username { unUser :: String }
  deriving (Read,Show,Ord,Eq,Typeable,Data)

deriveSafeCopy 1 'base ''Username
