{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Internal.Data.SessionKey where

import Data.Data
import System.Random
import Data.SafeCopy
import Happstack.Server.Internal.Types

-- | Abstract session identification
newtype SessionKey = SessionKey Integer
  deriving (Read,Show,Ord,Eq,Typeable,Data,Num,Random)

deriveSafeCopy 1 'base ''SessionKey

instance FromReqURI SessionKey where
  fromReqURI s = SessionKey `fmap` fromReqURI s
