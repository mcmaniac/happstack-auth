{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Internal.Data.SessionData where

import Data.Data
import Happstack.Data

import Happstack.Auth.Internal.Data.UserId
import Happstack.Auth.Internal.Data.Username

data SessionData = SessionData
    { sesUid        :: UserId
    , sesUsername   :: Username
    }
  deriving (Read,Show,Eq,Typeable,Data)

$(deriveSerialize ''SessionData)

instance Version SessionData
