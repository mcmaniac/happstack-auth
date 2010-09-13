{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Data.Internal.SaltedHash where

import Data.Data
import Happstack.Data
import Codec.Utils

newtype SaltedHash = SaltedHash [Octet]
  deriving (Read,Show,Ord,Eq,Typeable,Data)

$(deriveSerialize ''SaltedHash)

instance Version SaltedHash
