{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Data.Internal.Sessions where

import Data.Data
import Happstack.Data

import qualified Data.Map as M

import Happstack.Auth.Data.Internal.SessionKey

data Sessions a = Sessions { unsession :: M.Map SessionKey a }
  deriving (Read,Show,Eq,Typeable,Data)

$(deriveSerialize ''Sessions)

instance Version (Sessions a)
