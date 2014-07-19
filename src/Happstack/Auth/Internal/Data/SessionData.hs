{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving,
             TypeFamilies, MultiParamTypeClasses
             #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Happstack.Auth.Internal.Data.SessionData where

import Data.Typeable
import Data.ByteString
import Data.SafeCopy
import System.Time

import Happstack.Auth.Internal.Data.UserId
import Happstack.Auth.Internal.Data.Username

import qualified Happstack.Auth.Internal.Data.Old.SessionData0 as Old

data SessionData = SessionData
    { sesUid            :: UserId
    , sesUsername       :: Username
    , sesTimeout        :: ClockTime
    , sesFingerprint    :: (Either String ByteString, Maybe ByteString)
    }
  deriving (Show,Eq,Typeable)

deriveSafeCopy 2 'extension ''SessionData

instance Migrate SessionData where
    type MigrateFrom SessionData = Old.SessionData
    migrate (Old.SessionData uid un) = SessionData uid un (TOD 0 0) (Left "", Nothing)
