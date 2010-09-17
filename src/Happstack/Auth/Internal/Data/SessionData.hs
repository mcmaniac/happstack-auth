{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable,
             TypeFamilies, MultiParamTypeClasses
             #-}

module Happstack.Auth.Internal.Data.SessionData where

import Data.Data
import Data.ByteString
import Happstack.Data
import Happstack.State.ClockTime

import Happstack.Auth.Internal.Data.UserId
import Happstack.Auth.Internal.Data.Username

import qualified Happstack.Auth.Internal.Data.Old.SessionData0 as Old

data SessionData = SessionData
    { sesUid            :: UserId
    , sesUsername       :: Username
    , sesTimeout        :: ClockTime
    , sesFingerprint    :: (Either String ByteString, Maybe ByteString)
    }
  deriving (Show,Eq,Typeable,Data)

$(deriveSerialize ''SessionData)

instance Version SessionData where
    mode = extension 1 (Proxy :: Proxy Old.SessionData)

instance Migrate Old.SessionData SessionData where
    migrate (Old.SessionData uid un) = SessionData uid un (TOD 0 0) (Left "", Nothing)
