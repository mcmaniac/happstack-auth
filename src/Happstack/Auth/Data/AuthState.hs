{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Data.AuthState where

import Data.Data
import Happstack.Data
import Happstack.Data.IxSet
import Happstack.State

import qualified Data.Map as M

import Happstack.Auth.Data.SessionData
import Happstack.Auth.Data.Sessions
import Happstack.Auth.Data.User
import Happstack.Auth.Data.UserId

data AuthState = AuthState
    { sessions      :: Sessions SessionData
    , users         :: UserDB
    , nextUid       :: UserId
    }
  deriving (Show,Read,Typeable,Data)

instance Version AuthState

$(deriveSerialize ''AuthState)

instance Component AuthState where
  type Dependencies AuthState = End
  initialValue = AuthState (Sessions M.empty) empty 0
