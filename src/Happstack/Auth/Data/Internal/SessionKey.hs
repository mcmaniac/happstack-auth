{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Data.Internal.SessionKey where

import Data.Data
import System.Random
import Happstack.Data

-- | Abstract session identification
newtype SessionKey = SessionKey Integer
  deriving (Read,Show,Ord,Eq,Typeable,Data,Num,Random)

$(deriveSerialize ''SessionKey)

instance Version SessionKey
