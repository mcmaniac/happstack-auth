{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses,
             FlexibleContexts, FlexibleInstances, TupleSections
             #-}

module Happstack.Auth
    (
      -- * High level functions

      -- ** User registration
      register
    , changePassword

      -- ** Session management
    , performLogin
    , performLogout
    , loginHandler
    , logoutHandler
    , clearSessionCookie
    , getSessionData
    , getSessionKey
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
    , AuthState
    , authProxy
    ) where


import Control.Applicative
import Control.Monad.Reader
import Data.Maybe

import Data.Convertible
import Happstack.Server
import Happstack.State

import Happstack.Auth.Internal
import Happstack.Auth.Internal.Data hiding (Username, User, SessionData)
import qualified Happstack.Auth.Internal.Data as D


queryPolicy :: BodyPolicy
queryPolicy = defaultBodyPolicy "/tmp/happstack-auth" 0 4096 4096

sessionCookie :: String
sessionCookie = "sid"


--------------------------------------------------------------------------------
-- Avoid the whole newtype packing:

{- $datatypes

These data types collide with the data definitions used internaly in
"Happstack.Auth.Data.Internal". However, if you need both modules you might
want to import the Data module qualified:

> import Happstack.Auth
> import qualified Happstack.Auth.Data.Internal as AuthD

-}

--
-- Users:
--
type Username = String
type Password = String

data User = User
    { userId        :: UserId
    , userName      :: Username
    , userPass      :: SaltedHash
    }

fromDUser :: D.User -> User
fromDUser (D.User i (D.Username n) p) = User i n p

instance Convertible D.User User where
    safeConvert = Right . fromDUser

toDUser :: User -> D.User
toDUser (User i n p) = D.User i (D.Username n) p

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
fromDSession (D.SessionData i (D.Username n)) = SessionData i n

instance Convertible D.SessionData SessionData where
    safeConvert = Right . fromDSession

toDSession :: SessionData -> D.SessionData
toDSession (SessionData i n) = D.SessionData i (D.Username n)

instance Convertible SessionData D.SessionData where
    safeConvert = Right . toDSession


--
-- Auth Proxy
--

authProxy :: Proxy AuthState
authProxy = Proxy


--------------------------------------------------------------------------------
-- End user functions: Users

addUser :: (MonadIO m) => Username -> Password -> m (Maybe User)
addUser u p = do
    s <- liftIO $ buildSaltAndHash p
    case s of
         Just s' -> maybeUser . update $ AddUser (D.Username u) s'
         Nothing -> return Nothing

getUser :: (MonadIO m) => Username -> m (Maybe User)
getUser u = maybeUser . query $ GetUser (D.Username u)

getUserById :: (MonadIO m) => UserId -> m (Maybe User)
getUserById i = maybeUser . query $ GetUserById i

delUser :: (MonadIO m) => Username -> m ()
delUser u = update $ DelUser (D.Username u)

authUser :: (MonadIO m) => Username -> Password -> m (Maybe User)
authUser u p = maybeUser . query $ AuthUser u p

isUser :: (MonadIO m) => Username -> m Bool
isUser u = query $ IsUser (D.Username u)

listUsers :: (MonadIO m) => m [Username]
listUsers = query ListUsers >>= return . map D.unUser

numUsers :: (MonadIO m) => m Int
numUsers = query NumUsers

-- | Update (replace) a user
updateUser :: (MonadIO m) => User -> m ()
updateUser u = update $ UpdateUser (toDUser u)

-- | Warning: This `UserDB' uses the internal types from
-- "Happstack.Auth.Data.Internal"
askUsers :: (MonadIO m) => m UserDB
askUsers = query AskUsers


--------------------------------------------------------------------------------
-- End user functions: Sessions

clearAllSessions :: (MonadIO m) => m ()
clearAllSessions = update ClearAllSessions

setSession :: (MonadIO m) => SessionKey -> SessionData -> m ()
setSession k d = update $ SetSession k (toDSession d)

getSession :: (MonadIO m) => SessionKey -> m (Maybe SessionData)
getSession k = query (GetSession k) >>= return . fmap fromDSession

newSession :: (MonadIO m) => SessionData -> m SessionKey
newSession d = update $ NewSession (toDSession d)

delSession :: (MonadIO m) => SessionKey -> m ()
delSession k = update $ DelSession k

numSessions :: (MonadIO m) => m Int
numSessions = query $ NumSessions

-- | Warning: This `Sessions' uses the internal types from
-- "Happstack.Auth.Data.Internal"
getSessions :: (MonadIO m) => m (Sessions D.SessionData)
getSessions = query GetSessions


--------------------------------------------------------------------------------
-- Session managment

performLogin :: (MonadIO m, FilterMonad Response m, ServerMonad m)
             => User
             -> m a         -- ^ Run with modified headers, including the new session cookie
             -> m a
performLogin user action = do
  key <- newSession $ SessionData (userId user) (userName user)
  let cookie = mkCookie sessionCookie (show key)
  addCookie (2678400) $ mkCookie sessionCookie (show key)
  localRq (\r -> r { rqCookies = (rqCookies r) ++ [(sessionCookie, cookie)] }) action

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
                                    (\user -> performLogin user okR)
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

-- | Get the `SessionData' of the currently logged in user
getSessionData :: (MonadIO m, MonadPlus m, ServerMonad m)
               => m (Maybe SessionData)
getSessionData = withSessionId action
  where
    action (Just sid) = getSession sid
    action Nothing    = return Nothing

-- | Get the identifier for the current session
getSessionKey :: (MonadIO m, MonadPlus m, ServerMonad m)
              => m (Maybe SessionKey)
getSessionKey = withSessionId return

withSessionId :: (Read a, MonadIO m, MonadPlus m, ServerMonad m)
              => (Maybe a -> m r)
              -> m r
withSessionId = withDataFn queryPolicy getSessionId
  where
    getSessionId :: (Read a) => RqData (Maybe a)
    getSessionId = optional $ readCookieValue sessionCookie


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
register :: (MonadIO m, FilterMonad Response m, ServerMonad m)
         => m a          -- ^ User exists response
         -> m a          -- ^ Success response
         -> Username
         -> Password
         -> m a
register uExists good user pass = do
    u <- addUser user pass
    case u of
         Just u' -> performLogin u' good
         Nothing -> uExists

changePassword :: (MonadIO m)
               => Username
               -> Password          -- ^ Old password
               -> Password          -- ^ New password
               -> m Bool
changePassword user oldpass newpass = do
    mu <- authUser user oldpass
    h <- liftIO $ buildSaltAndHash newpass
    case (mu, h) of
         ((Just u), Just h') -> do
             updateUser u { userPass = h' }
             return True
         _ -> return False
