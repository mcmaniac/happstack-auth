{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies
             #-}

module Happstack.Auth.Internal.Data.UserId where

import Data.Data
import Data.Word
import Happstack.Data

-- | Abstract user identification
newtype UserId = UserId { unUid :: Word64 }
  deriving (Read,Show,Ord,Eq,Typeable,Data,Num)

$(deriveSerialize ''UserId)

instance Version UserId
