module Demo where

import Control.Applicative

import Happstack.Server
import Happstack.Auth
import Text.Blaze

import Templates

postPolicy :: BodyPolicy
postPolicy = defaultBodyPolicy "/tmp/happstack-auth-demo" 0 1024 1024

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
                      (seeOther "/happstack-auth" $ toResponse "Registration OK")
                      un pw
         _ -> demoResponse registerTemplate

demoLogin :: ServerPart Response
demoLogin = withSession (demoResponse . loggedInTemplate) $
    loginHandler Nothing Nothing
                 (seeOther "/happstack-auth" $ toResponse "Login OK")
                 loginH
  where
    loginH (Just u) p = demoResponse $ loginFailTemplate u p
    loginH _ _        = demoResponse loginTemplate


demoLogout :: ServerPart Response
demoLogout = logoutHandler (seeOther "/happstack-auth" $ toResponse "Logout OK")

demoStats :: ServerPart Response
demoStats = do
    nu <- numUsers
    ul <- listUsers
    ns <- numSessions
    demoResponse $ statsTemplate nu ul ns
