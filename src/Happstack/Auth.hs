{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses,
             FlexibleContexts, FlexibleInstances, DeriveDataTypeable
             #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

module Happstack.Auth where

import Control.Monad.Reader
import Control.Monad.State (modify,get,gets)
import Data.Char
import Data.Maybe
import Numeric
import System.Random

import qualified Data.Map as M

import Codec.Utils
import Data.ByteString.Internal
import Data.Digest.SHA512
import Happstack.Data.IxSet
import Happstack.Server
import Happstack.State

import Happstack.Auth.Data

queryPolicy :: BodyPolicy
queryPolicy = defaultBodyPolicy "" 0 0 0

sessionCookie = "sid"

saltLength = 16
strToOctets = listToOctets . (map c2w)
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

askUsers :: Query AuthState UserDB
askUsers = return . users =<< ask

askSessions :: Query AuthState (Sessions SessionData)
askSessions = return . sessions =<< ask

getUser :: Username -> Query AuthState (Maybe User)
getUser username = do
  udb <- askUsers
  return $ getOne $ udb @= username

getUserById :: UserId -> Query AuthState (Maybe User)
getUserById uid = do
  udb <- askUsers
  return $ getOne $ udb @= uid

modUsers :: (UserDB -> UserDB) -> Update AuthState ()
modUsers f = modify (\s -> (AuthState (sessions s) (f $ users s) (nextUid s)))

modSessions :: (Sessions SessionData -> Sessions SessionData) -> Update AuthState ()
modSessions f = modify (\s -> (AuthState (f $ sessions s) (users s) (nextUid s)))

getAndIncUid :: Update AuthState UserId
getAndIncUid = do
  uid <- gets nextUid
  modify (\s -> (AuthState (sessions s) (users s) (uid+1)))
  return uid

isUser :: Username -> Query AuthState Bool
isUser name = do
  us <- askUsers
  return $ isJust $ getOne $ us @= name

addUser :: Username -> SaltedHash -> Update AuthState (Maybe User)
addUser name pass = do
  s <- get
  let exists = isJust $ getOne $ (users s) @= name
  if exists
    then return Nothing
    else do u <- newUser name pass
            modUsers $ insert u
            return $ Just u
  where newUser u p = do uid <- getAndIncUid
                         return $ User uid u p

delUser :: Username -> Update AuthState ()
delUser name = modUsers del
  where del db = case getOne (db @= name) of
                   Just u -> delete u db
                   Nothing -> db

updateUser u = do modUsers (updateIx (userid u) u)

authUser :: String -> String -> Query AuthState (Maybe User)
authUser name pass = do
  udb <- askUsers
  let u = getOne $ udb @= (Username name)
  case u of
    (Just v) -> return $ if checkSalt pass (userpass v) then u else Nothing
    Nothing  -> return Nothing

listUsers :: Query AuthState [Username]
listUsers = do
  udb <- askUsers
  return $ map username $ toList udb

numUsers :: Query AuthState Int
numUsers = liftM length listUsers

setSession :: SessionKey -> SessionData -> Update AuthState ()
setSession key u = do
  modSessions $ Sessions . (M.insert key u) . unsession
  return ()

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

getSession :: SessionKey -> Query AuthState (Maybe SessionData)
getSession key = liftM ((M.lookup key) . unsession) askSessions

getSessions :: SessionKey -> Query AuthState (Sessions SessionData)
getSessions key = askSessions

numSessions:: Query AuthState Int
numSessions = liftM (M.size . unsession) askSessions

$(mkMethods ''AuthState ['askUsers, 'addUser, 'getUser, 'getUserById, 'delUser, 'authUser,
             'isUser, 'listUsers, 'numUsers, 'updateUser, 'clearAllSessions,
             'setSession, 'getSession, 'getSessions, 'newSession, 'delSession, 'numSessions])

{-
 - Login page
 -}

data UserAuthInfo = UserAuthInfo String String
instance FromData UserAuthInfo where
  fromData = liftM2 UserAuthInfo (look "username")
             (look "password" `mplus` return "nopassword")

performLogin user = do
  key <- update $ NewSession (SessionData (userid user) (username user))
  addCookie (2678400) (mkCookie sessionCookie (show key))

{-
 - Handles data from a login form to log the user in.  The form must supply
 - fields named "username" and "password".
 -}
loginHandler successResponse failResponse = withData queryPolicy handler
  where handler (UserAuthInfo user pass) = do
          mu <- query $ AuthUser user pass
          case mu of
            Just u -> do performLogin u
                         successResponse
            Nothing -> failResponse

{-
 - Logout page
 -}

performLogout sid = do
  clearSessionCookie
  update $ DelSession sid

logoutHandler target = withSessionId handler
  where handler (Just sid) = do
          performLogout sid
          target
        handler Nothing = target

{-
 - Registration page
 -}

data NewUserInfo = NewUserInfo String String String
instance FromData NewUserInfo where
  fromData = liftM3 NewUserInfo (look "username")
             (look "password" `mplus` return "nopassword")
             (look "password2" `mplus` return "nopassword2")

register user pass = do
  h <- liftIO $ buildSaltAndHash pass
  update $ AddUser user h

checkAndAdd uExists good user pass = do
  u <- register user pass
  case u of
    Just u' -> do performLogin u'
                  good
    Nothing -> uExists

newUserHandler existsOrInvalid nomatch succ = newUserHandler' existsOrInvalid nomatch (const succ)

{- newUserHandler' passes the username of just created account to
 - the success part. This can be used to initiate any data associated
 - with a user.
 -}
newUserHandler' existsOrInvalid nomatch succ = withData queryPolicy handler
  where handler (NewUserInfo user pass1 pass2)
          | not (saneUsername user) = existsOrInvalid
          | pass1 /= pass2 = nomatch
          | otherwise = checkAndAdd existsOrInvalid (succ (Username user)) (Username user) pass1
        saneUsername str = foldl1 (&&) $ map isAlphaNum str


{-
 - Handles data from a new user registration form.  The form must supply
 - fields named "username", "password", and "password2".
 -}
newAccountHandler noMatch uExists good (NewUserInfo user pass1 pass2)
  | pass1 == pass2 = checkAndAdd uExists good (Username user) pass1
  | otherwise = noMatch

changePassword :: (MonadIO m)
               => String
               -> String
               -> String
               -> m Bool
changePassword user oldpass newpass = do
  mu <- query $ AuthUser user oldpass
  case mu of
    (Just u) -> do h <- liftIO $ buildSaltAndHash newpass
                   update $ UpdateUser (u {userpass = h})
                   return True
    Nothing  -> return False

{-
 - Requiring a login
 -}

clearSessionCookie :: (FilterMonad Response m)
                   => m ()
clearSessionCookie = addCookie 0 (mkCookie sessionCookie "0")

getSessionId :: (Read a) => RqData (Maybe a)
getSessionId = liftM Just (readCookieValue sessionCookie) `mplus` return Nothing

withSessionId :: (Read a, MonadIO m, MonadPlus m, ServerMonad m)
              => (Maybe a -> m r)
              -> m r
withSessionId = withDataFn queryPolicy getSessionId

getLoggedInUser :: (MonadIO m, MonadPlus m, ServerMonad m)
                => m (Maybe SessionData)
getLoggedInUser = withSessionId action
  where action (Just sid) = query $ GetSession sid
        action Nothing    = return Nothing

withSession :: (MonadIO m)
            => (SessionData -> ServerPartT m a)
            -> ServerPartT m a
            -> ServerPartT m a
withSession f guestSPT = withSessionId action
  where action (Just sid) = (query $ GetSession sid) >>= (maybe noSession f)
        action Nothing    = guestSPT
        noSession = clearSessionCookie >> guestSPT

loginGate :: (MonadIO m)
          => ServerPartT m a
          -> ServerPartT m a
          -> ServerPartT m a
loginGate reg guest = withSession (\_ -> reg) guest

