{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Data.Internal.SessionData where

import Data.Data
import Happstack.Data

import Happstack.Auth.Data.Internal.UserId
import Happstack.Auth.Data.Internal.Username

data SessionData = SessionData
    { sesUid        :: UserId
    , sesUsername   :: Username
    }
  deriving (Read,Show,Eq,Typeable,Data)

$(deriveSerialize ''SessionData)

instance Version SessionData
