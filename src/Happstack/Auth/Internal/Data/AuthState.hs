{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Internal.Data.AuthState where

import Data.Typeable
import Data.SafeCopy

import Happstack.Auth.Internal.Data.SessionData
import Happstack.Auth.Internal.Data.Sessions
import Happstack.Auth.Internal.Data.User
import Happstack.Auth.Internal.Data.UserId

-- | Add this to your Dependency-List of your application state
data AuthState = AuthState
    { sessions      :: Sessions SessionData
    , users         :: UserDB
    , nextUid       :: UserId
    }
  deriving (Show, Typeable)

deriveSafeCopy 1 'base ''AuthState
