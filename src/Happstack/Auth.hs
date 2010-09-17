{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, MultiParamTypeClasses,
             FlexibleContexts, FlexibleInstances, TupleSections, CPP
             #-}

module Happstack.Auth
    (
      -- * High level functions

      -- ** User registration
      register
    , changePassword
    , setPassword

      -- ** Session management
    , updateTimeout
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
    , clearExpiredSessions

      -- * Data types
      -- $datatypes
    , User (), userName, userId
    , Username
    , Password
    , UserId
    , SessionData (..)
    , SessionKey
    , Minutes
    , AuthState
    , authProxy
    ) where


import Control.Applicative
import Control.Monad.Reader
import Data.Maybe
import System.Time

import qualified Data.ByteString.Char8 as BS8

import Data.Convertible
import Happstack.Server
import Happstack.State

import Happstack.Auth.Internal
import Happstack.Auth.Internal.Data hiding (Username, User, SessionData)
import qualified Happstack.Auth.Internal.Data as D


#if MIN_VERSION_happstack(0,5,1)
queryPolicy :: BodyPolicy
queryPolicy = defaultBodyPolicy "/tmp/happstack-auth" 0 4096 4096
#endif

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
    , _userPass     :: SaltedHash
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
    { sessionUserId         :: UserId
    , sessionUsername       :: Username
    , sessionTimeout        :: ClockTime
    , sessionFingerprint    :: (Either String BS8.ByteString, Maybe BS8.ByteString) -- ^ IP & user-agent
    }

fromDSession :: D.SessionData -> SessionData
fromDSession (D.SessionData i (D.Username n) t f) = SessionData i n t f

instance Convertible D.SessionData SessionData where
    safeConvert = Right . fromDSession

toDSession :: SessionData -> D.SessionData
toDSession (SessionData i n t f) = D.SessionData i (D.Username n) t f

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

getFingerprint :: (MonadIO m, ServerMonad m) => m (Either String BS8.ByteString, Maybe BS8.ByteString)
getFingerprint = do
    userAgent <- getHeaderM "user-agent"
    forwarded <- getHeaderM "x-forwarded-for"
    case forwarded of
         Just f  -> return (Right f, userAgent)
         Nothing -> do
             (ip, _) <- askRq >>= return . rqPeer
             return (Left ip, userAgent)

-- | Warning: This `Sessions' uses the internal types from
-- "Happstack.Auth.Data.Internal"
getSessions :: (MonadIO m) => m (Sessions D.SessionData)
getSessions = query GetSessions


--------------------------------------------------------------------------------
-- Session managment

type Minutes = Int


-- | Update the session timeout of logged in users. Add this to the top of your
-- application route, for example:
--
-- > appRoute :: ServerPart Response
-- > appRoute = updateTimeout 5 >> msum
-- >     [ {- your routing here -}
-- >     ]
updateTimeout :: (MonadIO m, FilterMonad Response m, MonadPlus m, ServerMonad m)
              => Minutes
              -> m ()
updateTimeout mins = withSessionId action
  where
    action Nothing    = return ()
    action (Just sid) = do
        c <- liftIO getClockTime
        let c'     = addToClockTime noTimeDiff { tdMin = mins } c
            cookie = mkCookie sessionCookie (show sid)
        update $ UpdateTimeout sid c'
        addCookie (mins * 60) cookie


performLogin :: (MonadIO m, FilterMonad Response m, ServerMonad m)
             => Minutes     -- ^ Session timeout
             -> User
             -> m a         -- ^ Run with modified headers, including the new session cookie
             -> m a
performLogin mins user action = do

    f <- getFingerprint
    c <- liftIO getClockTime
    let clock = addToClockTime noTimeDiff { tdMin = mins } c
    key <- newSession $ SessionData (userId user) (userName user) clock f

    -- addCookie (2678400) $ mkCookie sessionCookie (show key)
    let cookie = mkCookie sessionCookie (show key)
    addCookie (mins * 60) cookie

    localRq (\r -> r { rqCookies = (rqCookies r) ++ [(sessionCookie, cookie)] }) action

-- | Handles data from a login form to log the user in.
loginHandler :: (MonadIO m, FilterMonad Response m, MonadPlus m, ServerMonad m)
             => Minutes                                         -- ^ Session timeout
             -> Maybe String                                    -- ^ POST field to look for username (default: \"username\")
             -> Maybe String                                    -- ^ POST field to look for password (default: \"password\")
             -> m a                                             -- ^ Success response
             -> (Maybe Username -> Maybe Password -> m a)       -- ^ Fail response. Arguments: Post data
             -> m a
loginHandler mins muname mpwd okR failR = do
#if MIN_VERSION_happstack(0,5,1)
    dat <- getDataFn queryPolicy . body $ do
#else
    dat <- getDataFn $ do
#endif
        un <- look            $ fromMaybe "username" muname
        pw <- optional . look $ fromMaybe "password" mpwd
        return (un,pw)

    case dat of
         Right (u, Just p) -> authUser u p
                          >>= maybe (failR (Just u) (Just p))
                                    (\user -> performLogin mins user okR)
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

clearExpiredSessions :: (MonadIO m) => m ()
clearExpiredSessions = liftIO getClockTime >>= update . ClearExpiredSessions


-- | Get the `SessionData' of the currently logged in user
getSessionData :: (MonadIO m, MonadPlus m, ServerMonad m)
               => m (Maybe SessionData)
getSessionData = do
    d <- withSessionId action
    f <- getFingerprint
    case d of
         Just sd | f == sessionFingerprint sd ->
             return $ Just sd
         _ ->
             return Nothing
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
withSessionId f = do
    clearExpiredSessions
    withDataFn queryPolicy getSessionId f
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
         => Minutes         -- ^ Session timeout
         -> Username
         -> Password
         -> m a             -- ^ User exists response
         -> m a             -- ^ Success response
         -> m a
register mins user pass uExists good = do
    u <- addUser user pass
    case u of
         Just u' -> performLogin mins u' good
         Nothing -> uExists

changePassword :: (MonadIO m)
               => Username
               -> Password          -- ^ Old password
               -> Password          -- ^ New password
               -> m Bool
changePassword user oldpass newpass = do
    ms <- liftIO $ buildSaltAndHash newpass
    case ms of
         Just s -> update $ ChangePassword user oldpass s
         _      -> return False

setPassword :: MonadIO m
            => Username
            -> Password
            -> m Bool
setPassword un p = do
    ms <- liftIO $ buildSaltAndHash p
    case ms of
         Just s -> update $ SetPassword (D.Username un) s
         _      -> return False
