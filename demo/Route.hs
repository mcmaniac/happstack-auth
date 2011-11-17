module Route where

import Control.Monad

import Happstack.Auth
import Happstack.Server hiding (timeout)

import Demo

appRoute :: ServerPartT IO Response
appRoute = updateTimeout timeout >> msum
    [ dir "happstack-auth" $ msum
        [ nullDir >>      demoHome
        , dir' "register" demoRegister
        , dir' "login"    demoLogin
        , dir' "logout"   demoLogout
        , dir' "stats"    demoStats
        , serveDirectory EnableBrowsing [] "."
        ]
    , nullDir >> seeOther "/happstack-auth" (toResponse "")
    ]
  where
    dir' p r = dir p (nullDir >> r)
