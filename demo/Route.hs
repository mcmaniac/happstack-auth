module Route where

import Control.Monad

import Happstack.Server

import Demo

appRoute :: ServerPart Response
appRoute = msum
    [ dir "happstack-auth" $ msum
        [ nullDir >>      demoHome
        , dir' "register" demoRegister
        , dir' "login"    demoLogin
        , dir' "logout"   demoLogout
        , dir' "stats"    demoStats
        , fileServe [] "."
        ]
    , seeOther "/happstack-auth" (toResponse "")
    ]
  where
    dir' p r = dir p (nullDir >> r)
