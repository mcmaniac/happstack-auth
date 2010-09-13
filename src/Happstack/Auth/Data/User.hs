{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}


module Happstack.Auth.Data.User where

import Data.Data
import Happstack.Data
import Happstack.Data.IxSet

import Happstack.Auth.Data.SaltedHash
import Happstack.Auth.Data.UserId
import Happstack.Auth.Data.Username

data User = User
    { userid        :: UserId
    , username      :: Username
    , userpass      :: SaltedHash
    }
  deriving (Read,Show,Ord,Eq,Typeable,Data)

$(deriveSerialize ''User)

instance Version User

$(inferIxSet "UserDB" ''User 'noCalcs [''UserId, ''Username])
