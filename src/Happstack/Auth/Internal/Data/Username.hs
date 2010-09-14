{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Internal.Data.Username where

import Data.Data
import Happstack.Data

newtype Username = Username { unUser :: String }
  deriving (Read,Show,Ord,Eq,Typeable,Data)

$(deriveSerialize ''Username)

instance Version Username
