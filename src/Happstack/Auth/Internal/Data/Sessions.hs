{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Internal.Data.Sessions where

import Data.Data
import Happstack.Data

import qualified Data.Map as M

import Happstack.Auth.Internal.Data.SessionKey

data Sessions a = Sessions { unsession :: M.Map SessionKey a }
  deriving (Read,Show,Eq,Typeable,Data)

$(deriveSerialize ''Sessions)

instance Version (Sessions a)
