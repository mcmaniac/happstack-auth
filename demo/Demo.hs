module Demo where

import Control.Applicative

import Happstack.Server
import Happstack.Auth
import Text.Blaze

import Templates

postPolicy :: BodyPolicy
postPolicy = defaultBodyPolicy "/tmp/happstack-auth-demo" 0 1024 1024

-- Session timeouts
timeout :: Minutes
timeout = 5

demoResponse :: Html        -- ^ Body
             -> ServerPartT IO Response
demoResponse html = do
    maybeSession <- getSessionData
    uri          <- rqUri <$> askRq
    ok . toResponse $
        defaultTemplate (defaultHeader Nothing)
                        (defaultBody   maybeSession uri html)


--------------------------------------------------------------------------------
-- Response handler

demoHome :: ServerPartT IO Response
demoHome = demoResponse homeTemplate

demoRegister :: ServerPartT IO Response
demoRegister = withSession (demoResponse . loggedInTemplate) $ do
    dat <- getDataFn postPolicy . body $ (,) <$> look "username"
                                             <*> look "password"
    case dat of
         Right (un,pw) -> do
             register timeout un pw
                      (demoResponse $ invalidUsernameTemplate un)
                      (seeOther "/happstack-auth" $ toResponse "Registration OK")
         _ -> demoResponse registerTemplate

demoLogin :: ServerPartT IO Response
demoLogin = withSession (demoResponse . loggedInTemplate) $
    loginHandler timeout Nothing Nothing
                 (seeOther "/happstack-auth" $ toResponse "Login OK")
                 loginH
  where
    loginH (Just u) p = demoResponse $ loginFailTemplate u p
    loginH _ _        = demoResponse loginTemplate


demoLogout :: ServerPartT IO Response
demoLogout = logoutHandler (seeOther "/happstack-auth" $ toResponse "Logout OK")

demoStats :: ServerPartT IO Response
demoStats = do
    nu <- numUsers
    ul <- listUsers
    ns <- numSessions
    demoResponse $ statsTemplate nu ul ns
