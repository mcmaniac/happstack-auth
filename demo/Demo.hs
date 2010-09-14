module Demo where

import Control.Applicative

import Happstack.Server
import Happstack.Auth
import Text.Blaze

import Templates

postPolicy :: BodyPolicy
postPolicy = defaultBodyPolicy "/tmp/happstack-auth-demo" 1024 1024 1024

demoResponse :: Html        -- ^ Body
             -> ServerPart Response
demoResponse html = do
    maybeSession <- getSessionData
    uri          <- rqUri <$> askRq
    ok . toResponse $
        defaultTemplate (defaultHeader Nothing)
                        (defaultBody   maybeSession uri html)


--------------------------------------------------------------------------------
-- Response handler

demoHome :: ServerPart Response
demoHome = demoResponse homeTemplate

demoRegister :: ServerPart Response
demoRegister = withSession (demoResponse . loggedInTemplate) $ do
    dat <- getDataFn postPolicy . body $ (,) <$> look "username"
                                             <*> look "password"
    case dat of
         Right (un,pw) -> do
             register (demoResponse $ invalidUsernameTemplate un)
                      demoHome
                      un pw
         _ -> demoResponse registerTemplate

demoLogin :: ServerPart Response
demoLogin = withSession (demoResponse . loggedInTemplate) $
    loginHandler Nothing Nothing
                 demoStats
                 loginH
  where
    loginH (Just u) Nothing  = demoResponse $ loginFailTemplate u Nothing
    loginH (Just u) (Just p) = demoResponse $ loginFailTemplate u (Just p)
    loginH _ _               = demoResponse loginTemplate


demoLogout :: ServerPart Response
demoLogout = logoutHandler demoHome

demoStats :: ServerPart Response
demoStats = do
    nu <- numUsers
    ul <- listUsers
    ns <- numSessions
    demoResponse $ statsTemplate nu ul ns
