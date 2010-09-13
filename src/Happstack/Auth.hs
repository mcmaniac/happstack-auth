{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses,
             FlexibleContexts, FlexibleInstances, DeriveDataTypeable
             #-}
{-# OPTIONS -fno-warn-orphans #-}

module Happstack.Auth
    (
      -- * High level functions

      -- ** User registration
    , register
    , changePassword

      -- ** Session management
      performLogin
    , performLogout
    , loginHandler
    , logoutHandler
    , clearSessionCookie
    , getLoggedInUser
    , withSession
    , loginGate

      -- * Basic functions

      -- ** Users
    , addUser
    , getUser
    , getUserById
    , delUser
    , updateUser
    , authUser
    , isUser
    , listUsers
    , numUsers
    , askUsers

      -- ** Sessions
    , newSession
    , getSession
    , setSession
    , delSession
    , clearAllSessions
    , numSessions
    , getSessions


      -- * Data types
      -- $datatypes
    , User (), userName, userId
    , Username
    , Password
    , UserId
    , SessionData (..)
    , SessionKey
    ) where


import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State (modify,get,gets)
import Data.Maybe
import Data.Word
import Numeric
import System.Random

import qualified Data.Map as M

import Data.Convertible
import Codec.Utils
import Data.ByteString.Internal
import Data.Digest.SHA512
import Happstack.Data.IxSet
import Happstack.Server
import Happstack.State

import Happstack.Auth.Data hiding (Username, UserId, User, SessionData)
import qualified Happstack.Auth.Data as D


queryPolicy :: BodyPolicy
queryPolicy = defaultBodyPolicy "" 0 0 0

sessionCookie :: String
sessionCookie = "sid"


--------------------------------------------------------------------------------
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

sAskUsers :: Query AuthState UserDB
sAskUsers = return . users =<< ask

sAskSessions :: Query AuthState (Sessions D.SessionData)
sAskSessions = return . sessions =<< ask

sGetUser :: D.Username -> Query AuthState (Maybe D.User)
sGetUser un = do
  udb <- sAskUsers
  return $ getOne $ udb @= un

sGetUserById :: D.UserId -> Query AuthState (Maybe D.User)
sGetUserById uid = do
  udb <- sAskUsers
  return $ getOne $ udb @= uid

sModUsers :: (UserDB -> UserDB) -> Update AuthState ()
sModUsers f = modify (\s -> (AuthState (sessions s) (f $ users s) (nextUid s)))

sModSessions :: (Sessions D.SessionData -> Sessions D.SessionData) -> Update AuthState ()
sModSessions f = modify (\s -> (AuthState (f $ sessions s) (users s) (nextUid s)))

sGetAndIncUid :: Update AuthState D.UserId
sGetAndIncUid = do
  uid <- gets nextUid
  modify (\s -> (AuthState (sessions s) (users s) (uid+1)))
  return uid

sIsUser :: D.Username -> Query AuthState Bool
sIsUser name = do
  us <- sAskUsers
  return $ isJust $ getOne $ us @= name

sAddUser :: D.Username -> SaltedHash -> Update AuthState (Maybe D.User)
sAddUser name pass = do
  s <- get
  let exists = isJust $ getOne $ (users s) @= name
  if exists
    then return Nothing
    else do u <- newUser name pass
            sModUsers $ insert u
            return $ Just u
  where newUser u p = do uid <- sGetAndIncUid
                         return $ D.User uid u p

sDelUser :: D.Username -> Update AuthState ()
sDelUser name = sModUsers del
  where del db = case getOne (db @= name) of
                   Just u -> delete u db
                   Nothing -> db

sUpdateUser :: D.User -> Update AuthState ()
sUpdateUser u = sModUsers (updateIx (userid u) u)

sAuthUser :: String -> String -> Query AuthState (Maybe D.User)
sAuthUser name pass = do
  udb <- sAskUsers
  let u = getOne $ udb @= (D.Username name)
  case u of
    (Just v) -> return $ if checkSalt pass (userpass v) then u else Nothing
    Nothing  -> return Nothing

sListUsers :: Query AuthState [D.Username]
sListUsers = do
  udb <- sAskUsers
  return $ map username $ toList udb

sNumUsers :: Query AuthState Int
sNumUsers = liftM length sListUsers

sSetSession :: SessionKey -> D.SessionData -> Update AuthState ()
sSetSession key u = do
  sModSessions $ Sessions . (M.insert key u) . unsession
  return ()

sNewSession :: D.SessionData -> Update AuthState SessionKey
sNewSession u = do
  key <- getRandom
  sSetSession key u
  return key

sDelSession :: SessionKey -> Update AuthState ()
sDelSession key = do
  sModSessions $ Sessions . (M.delete key) . unsession
  return ()

sClearAllSessions :: Update AuthState ()
sClearAllSessions = sModSessions $ const (Sessions M.empty)

sGetSession :: SessionKey -> Query AuthState (Maybe D.SessionData)
sGetSession key = liftM ((M.lookup key) . unsession) sAskSessions

sGetSessions :: Query AuthState (Sessions D.SessionData)
sGetSessions = sAskSessions

sNumSessions:: Query AuthState Int
sNumSessions = liftM (M.size . unsession) sAskSessions

$(mkMethods ''AuthState
    [ 'sAskUsers
    , 'sAddUser
    , 'sGetUser
    , 'sGetUserById
    , 'sDelUser
    , 'sAuthUser
    , 'sIsUser
    , 'sListUsers
    , 'sNumUsers
    , 'sUpdateUser

    , 'sClearAllSessions
    , 'sSetSession
    , 'sGetSession
    , 'sGetSessions
    , 'sNewSession
    , 'sDelSession
    , 'sNumSessions
    ])


--------------------------------------------------------------------------------
-- Avoid the whole newtype packing:

{- $datatypes

These data types collide with the data definitions used internaly in
"Happstack.Auth.Data". However, if you need both modules you might want to
import the Data module qualified:

> import Happstack.Auth
> import qualified Happstack.Auth.Data as AuthD

-}

--
-- Users:
--
type Username = String
type Password = String
type UserId   = Word64

data User = User
    { userId        :: UserId
    , userName      :: Username
    , userPass      :: SaltedHash
    }

fromDUser :: D.User -> User
fromDUser (D.User (D.UserId i) (D.Username n) p) = User i n p

instance Convertible D.User User where
    safeConvert = Right . fromDUser

toDUser :: User -> D.User
toDUser (User i n p) = D.User (D.UserId i) (D.Username n) p

instance Convertible User D.User where
    safeConvert = Right . toDUser


maybeUser :: MonadIO m => m (Maybe D.User) -> m (Maybe User)
maybeUser m = m >>= return . fmap fromDUser

--
-- Sessions:
--

data SessionData = SessionData
    { sessionUserId     :: UserId
    , sessionUsername   :: Username
    }

fromDSession :: D.SessionData -> SessionData
fromDSession (D.SessionData (D.UserId i) (D.Username n)) = SessionData i n

instance Convertible D.SessionData SessionData where
    safeConvert = Right . fromDSession

toDSession :: SessionData -> D.SessionData
toDSession (SessionData i n) = D.SessionData (D.UserId i) (D.Username n)

instance Convertible SessionData D.SessionData where
    safeConvert = Right . toDSession


--------------------------------------------------------------------------------
-- End user functions: Users

addUser :: (MonadIO m) => Username -> Password -> m (Maybe User)
addUser u p = do
    s <- liftIO $ buildSaltAndHash p
    maybeUser . update $ SAddUser (D.Username u) s

getUser :: (MonadIO m) => Username -> m (Maybe User)
getUser u = maybeUser . query $ SGetUser (D.Username u)

getUserById :: (MonadIO m) => UserId -> m (Maybe User)
getUserById i = maybeUser . query $ SGetUserById (D.UserId i)

delUser :: (MonadIO m) => Username -> m ()
delUser u = update $ SDelUser (D.Username u)

authUser :: (MonadIO m) => Username -> Password -> m (Maybe User)
authUser u p = maybeUser . query $ SAuthUser u p

isUser :: (MonadIO m) => Username -> m Bool
isUser u = query $ SIsUser (D.Username u)

listUsers :: (MonadIO m) => m [Username]
listUsers = query SListUsers >>= return . map D.unUser

numUsers :: (MonadIO m) => m Int
numUsers = query SNumUsers

-- | Update (replace) a user
updateUser :: (MonadIO m) => User -> m ()
updateUser u = update $ SUpdateUser (toDUser u)

-- | Warning: This `UserDB' uses the internal types from "Happstack.Auth.Data"
askUsers :: (MonadIO m) => m UserDB
askUsers = query SAskUsers


--------------------------------------------------------------------------------
-- End user functions: Sessions

clearAllSessions :: (MonadIO m) => m ()
clearAllSessions = update SClearAllSessions

setSession :: (MonadIO m) => SessionKey -> SessionData -> m ()
setSession k d = update $ SSetSession k (toDSession d)

getSession :: (MonadIO m) => SessionKey -> m (Maybe SessionData)
getSession k = query (SGetSession k) >>= return . fmap fromDSession

newSession :: (MonadIO m) => SessionData -> m SessionKey
newSession d = update $ SNewSession (toDSession d)

delSession :: (MonadIO m) => SessionKey -> m ()
delSession k = update $ SDelSession k

numSessions :: (MonadIO m) => m Int
numSessions = query $ SNumSessions

-- | Warning: This `Sessions' uses the internal types from
-- "Happstack.Auth.Data"
getSessions :: (MonadIO m) => m (Sessions D.SessionData)
getSessions = query SGetSessions


--------------------------------------------------------------------------------
-- Session managment

performLogin :: (MonadIO m, FilterMonad Response m)
             => User
             -> m ()
performLogin user = do
  key <- newSession $ SessionData (userId user) (userName user)
  addCookie (2678400) $ mkCookie sessionCookie (show key)

-- | Handles data from a login form to log the user in.
loginHandler :: (MonadIO m, FilterMonad Response m, MonadPlus m, ServerMonad m)
             => Maybe String                                    -- ^ POST field to look for username (default: \"username\")
             -> Maybe String                                    -- ^ POST field to look for password (default: \"password\")
             -> m a                                             -- ^ Success response
             -> (Maybe Username -> Maybe Password -> m a)       -- ^ Fail response. Arguments: Post data
             -> m a
loginHandler muname mpwd okR failR = do
    dat <- getDataFn queryPolicy . body $ do
        un <- look            $ fromMaybe "username" muname
        pw <- optional . look $ fromMaybe "password" mpwd
        return (un,pw)

    case dat of
         Right (u, Just p) -> authUser u p
                          >>= maybe (failR (Just u) (Just p))
                                    (\user -> performLogin user >> okR)
         Right (u, mp)     -> failR (Just u) mp
         _                 -> failR Nothing Nothing


performLogout :: (MonadIO m, FilterMonad Response m) => SessionKey -> m ()
performLogout sid = do
    clearSessionCookie
    delSession sid


logoutHandler :: (ServerMonad m, MonadPlus m, MonadIO m, FilterMonad Response m)
              => m a    -- ^ Response after logout
              -> m a
logoutHandler target = withSessionId handler
  where
    handler (Just sid) = do
        performLogout sid
        target
    handler Nothing = target


clearSessionCookie :: (FilterMonad Response m) => m ()
clearSessionCookie = addCookie 0 (mkCookie sessionCookie "0")

getSessionId :: (Read a) => RqData (Maybe a)
getSessionId = optional $ readCookieValue sessionCookie

-- | Get the `SessionData' of the currently logged in user
getLoggedInUser :: (MonadIO m, MonadPlus m, ServerMonad m)
                => m (Maybe SessionData)
getLoggedInUser = withSessionId action
  where
    action (Just sid) = getSession sid
    action Nothing    = return Nothing


-- Not sure what we need this for?
withSessionId :: (Read a, MonadIO m, MonadPlus m, ServerMonad m)
              => (Maybe a -> m r)
              -> m r
withSessionId = withDataFn queryPolicy getSessionId


-- | Run a `ServerPartT' with the `SessionData' of the currently logged in user
-- (if available)
withSession :: (MonadIO m)
            => (SessionData -> ServerPartT m a)     -- ^ Logged in response
            -> ServerPartT m a                      -- ^ Not logged in response
            -> ServerPartT m a
withSession f guestSPT = withSessionId action
  where
    action (Just sid) = getSession sid >>= maybe noSession f
    action Nothing    = guestSPT
    noSession         = clearSessionCookie >> guestSPT


-- | Require a login
loginGate :: (MonadIO m)
          => ServerPartT m a    -- ^ Logged in
          -> ServerPartT m a    -- ^ Not registered
          -> ServerPartT m a
loginGate reg guest = withSession (\_ -> reg) guest


--------------------------------------------------------------------------------
-- User registration

-- | Register a new user
register :: (MonadIO m, FilterMonad Response m)
         => m a          -- ^ User exists response
         -> m a          -- ^ Success response
         -> Username
         -> Password
         -> m a
register uExists good user pass = do
    u <- addUser user pass
    case u of
         Just u' -> performLogin u' >> good
         Nothing -> uExists

changePassword :: (MonadIO m)
               => Username
               -> Password          -- ^ Old password
               -> Password          -- ^ New password
               -> m Bool
changePassword user oldpass newpass = do
    mu <- authUser user oldpass
    case mu of
         (Just u) -> do h <- liftIO $ buildSaltAndHash newpass
                        updateUser u { userPass = h }
                        return True
         Nothing  -> return False
