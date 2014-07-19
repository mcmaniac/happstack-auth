{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Internal.Data.SaltedHash where

import Data.Data
import Data.SafeCopy
import Codec.Utils

newtype SaltedHash = SaltedHash [Octet]
  deriving (Read,Show,Ord,Eq,Typeable,Data)

deriveSafeCopy 1 'base ''SaltedHash
