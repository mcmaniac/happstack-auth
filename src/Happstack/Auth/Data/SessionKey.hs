{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Data.SessionKey where

import Data.Data
import Random
import Happstack.Data

newtype SessionKey = SessionKey Integer
  deriving (Read,Show,Ord,Eq,Typeable,Data,Num,Random)

$(deriveSerialize ''SessionKey)

instance Version SessionKey
