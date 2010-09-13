{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}


module Happstack.Auth.Data.Internal.User where

import Data.Data
import Happstack.Data
import Happstack.Data.IxSet

import Happstack.Auth.Data.Internal.SaltedHash
import Happstack.Auth.Data.Internal.UserId
import Happstack.Auth.Data.Internal.Username

data User = User
    { userid        :: UserId
    , username      :: Username
    , userpass      :: SaltedHash
    }
  deriving (Read,Show,Ord,Eq,Typeable,Data)

$(deriveSerialize ''User)

instance Version User

$(inferIxSet "UserDB" ''User 'noCalcs [''UserId, ''Username])
