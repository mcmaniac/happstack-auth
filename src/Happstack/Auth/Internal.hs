{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses,
             FlexibleContexts, FlexibleInstances, DeriveDataTypeable
             #-}
{-# OPTIONS -fno-warn-orphans #-}

module Happstack.Auth.Internal
    ( buildSaltAndHash

    , AskUsers (..)
    , AddUser (..)
    , GetUser (..)
    , GetUserById (..)
    , DelUser (..)
    , AuthUser (..)
    , IsUser (..)
    , ListUsers (..)
    , NumUsers (..)
    , UpdateUser (..)

    , ClearAllSessions (..)
    , SetSession (..)
    , GetSession (..)
    , GetSessions (..)
    , NewSession (..)
    , DelSession (..)
    , NumSessions (..)
    ) where


import Control.Monad.Reader
import Control.Monad.State (modify,get,gets)
import Data.Maybe
import Numeric
import System.Random

import qualified Data.Map as M

import Codec.Utils
import Data.ByteString.Internal
import Data.Digest.SHA512
import Happstack.Data.IxSet
import Happstack.State

import Happstack.Auth.Internal.Data hiding (Username, User, SessionData)
import qualified Happstack.Auth.Internal.Data as D


-------------------------------------------------------------------------------
-- Password generation

saltLength :: Num t => t
saltLength = 16

strToOctets :: String -> [Octet]
strToOctets = listToOctets . (map c2w)

slowHash :: [Octet] -> [Octet]
slowHash a = (iterate hash a) !! 512

randomSalt :: IO String
randomSalt = liftM concat $ sequence $ take saltLength $ repeat $
  randomRIO (0::Int,15) >>= return . flip showHex ""

buildSaltAndHash :: String -> IO SaltedHash
buildSaltAndHash str = do
  salt <- randomSalt
  let salt' = strToOctets salt
  let str' = strToOctets str
  let h = slowHash (salt'++str')
  return $ SaltedHash $ salt'++h

checkSalt :: String -> SaltedHash -> Bool
checkSalt str (SaltedHash h) = h == salt++(slowHash $ salt++(strToOctets str))
  where salt = take saltLength h


--------------------------------------------------------------------------------
-- State functions: Users

askUsers :: Query AuthState UserDB
askUsers = return . users =<< ask

askSessions :: Query AuthState (Sessions D.SessionData)
askSessions = return . sessions =<< ask

getUser :: D.Username -> Query AuthState (Maybe D.User)
getUser un = do
  udb <- askUsers
  return $ getOne $ udb @= un

getUserById :: D.UserId -> Query AuthState (Maybe D.User)
getUserById uid = do
  udb <- askUsers
  return $ getOne $ udb @= uid

modUsers :: (UserDB -> UserDB) -> Update AuthState ()
modUsers f = modify (\s -> (AuthState (sessions s) (f $ users s) (nextUid s)))

modSessions :: (Sessions D.SessionData -> Sessions D.SessionData) -> Update AuthState ()
modSessions f = modify (\s -> (AuthState (f $ sessions s) (users s) (nextUid s)))

getAndIncUid :: Update AuthState D.UserId
getAndIncUid = do
  uid <- gets nextUid
  modify (\s -> (AuthState (sessions s) (users s) (uid+1)))
  return uid

isUser :: D.Username -> Query AuthState Bool
isUser name = do
  us <- askUsers
  return $ isJust $ getOne $ us @= name

addUser :: D.Username -> SaltedHash -> Update AuthState (Maybe D.User)
addUser name pass = do
  s <- get
  let exists = isJust $ getOne $ (users s) @= name
  if exists
    then return Nothing
    else do u <- newUser name pass
            modUsers $ insert u
            return $ Just u
  where newUser u p = do uid <- getAndIncUid
                         return $ D.User uid u p

delUser :: D.Username -> Update AuthState ()
delUser name = modUsers del
  where del db = case getOne (db @= name) of
                   Just u -> delete u db
                   Nothing -> db

updateUser :: D.User -> Update AuthState ()
updateUser u = modUsers (updateIx (userid u) u)

authUser :: String -> String -> Query AuthState (Maybe D.User)
authUser name pass = do
  udb <- askUsers
  let u = getOne $ udb @= (D.Username name)
  case u of
    (Just v) -> return $ if checkSalt pass (userpass v) then u else Nothing
    Nothing  -> return Nothing

listUsers :: Query AuthState [D.Username]
listUsers = do
  udb <- askUsers
  return $ map username $ toList udb

numUsers :: Query AuthState Int
numUsers = liftM length listUsers


--------------------------------------------------------------------------------
-- State functions: Sessions

setSession :: SessionKey -> D.SessionData -> Update AuthState ()
setSession key u = do
  modSessions $ Sessions . (M.insert key u) . unsession
  return ()

newSession :: D.SessionData -> Update AuthState SessionKey
newSession u = do
  key <- getRandom
  setSession key u
  return key

delSession :: SessionKey -> Update AuthState ()
delSession key = do
  modSessions $ Sessions . (M.delete key) . unsession
  return ()

clearAllSessions :: Update AuthState ()
clearAllSessions = modSessions $ const (Sessions M.empty)

getSession :: SessionKey -> Query AuthState (Maybe D.SessionData)
getSession key = liftM ((M.lookup key) . unsession) askSessions

getSessions :: Query AuthState (Sessions D.SessionData)
getSessions = askSessions

numSessions:: Query AuthState Int
numSessions = liftM (M.size . unsession) askSessions


--------------------------------------------------------------------------------
-- Generate Methods

$(mkMethods ''AuthState
    [ 'askUsers
    , 'addUser
    , 'getUser
    , 'getUserById
    , 'delUser
    , 'authUser
    , 'isUser
    , 'listUsers
    , 'numUsers
    , 'updateUser

    , 'clearAllSessions
    , 'setSession
    , 'getSession
    , 'getSessions
    , 'newSession
    , 'delSession
    , 'numSessions
    ])
