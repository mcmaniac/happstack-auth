{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies, MultiParamTypeClasses
             #-}


module Happstack.Auth.Internal.Data.User where

import Data.Data
import Data.SafeCopy
import Data.IxSet

import Happstack.Auth.Internal.Data.SaltedHash
import Happstack.Auth.Internal.Data.UserId
import Happstack.Auth.Internal.Data.Username

data User = User
    { userid        :: UserId
    , username      :: Username
    , userpass      :: SaltedHash
    }
  deriving (Read,Show,Ord,Eq,Typeable,Data)

deriveSafeCopy 1 'base ''User

inferIxSet "UserDB" ''User 'noCalcs [''UserId, ''Username]
