{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Templates where

import Prelude hiding (head, div, id, span)
import Data.Maybe
import Data.ByteString.Char8 (unpack)

import Text.Blaze
import Text.Blaze.Html5 hiding (map, style)
import Text.Blaze.Html5.Attributes hiding (title, span, form, label)

import Happstack.Auth


--------------------------------------------------------------------------------
-- Home template

homeTemplate :: Html
homeTemplate = do

    h1 ! class_ "label-green" $ "Welcome to Happstack-Auth!"

    p $ do
        "Happstack-Auth is an authentication suite for the "
        a ! href "http://www.haskell.org" $ "Haskell"
        " web framework "
        a ! href "http://www.happstack.com" $ "Happstack"
        "."

    p $ do
        "This website is for demonstrating purposes only. Feel free to register "
        -- em "(currently disabled) "
        "and login to see some statistics."


    h2 ! class_ "label-yellow" $ "Using Happstack-Auth"

    p $ do
        "You can take a look at the "
        a ! href "http://github.com/mcmaniac/happstack-auth/tree/master/demo/" $ "source code"
        " of this website to see a working example."

    p $ "Happstack-Auth is installed via cabal:"
    div ! class_ "code" $ code $ do
        "$ git clone git://github.com/mcmaniac/happstack-auth.git"
        br
        "$ cd happstack-auth"
        br
        "$ cabal install"

    p "Run the demo via runghc and open your browser at \"http://localhost:8080\":"
    div ! class_ "code" $ code $ do
        "$ cd demo"
        br
        "$ runghc Main.hs"

    p $ do
        "To learn more about Happstack-Auth and Happstack in general follow the links on the "
        "\"Quick Links\" section of the sidebar on the right."


    h2 ! class_ "label-yellow" $ "Developing Happstack-Auth"

    p "Happstack-Auth is an unofficial Happstack module, maintained by Nils Schweinsberg."
    p $ do
        "If you discover any bugs or have a feature request, just send me a "
        " quick email or contact me via "
        a ! href "http://github.com/mcmaniac" $ "github.com"
        "."


--------------------------------------------------------------------------------
-- Login Status templates

loggedInTemplate :: SessionData -> Html
loggedInTemplate (SessionData _ un _ _) = do
    h1 ! class_ "label-red" $ "Already logged in."
    p $ toHtml $ "You are already logged in as " ++ un ++ "."

loginForm :: Maybe Username -> Html
loginForm un =
    form ! action "/happstack-auth/login" ! method "post" ! enctype "multipart/form-data" $ do

        label "Username:"
        input ! type_ "text"     ! name "username" ! value (toValue $ fromMaybe "" un)
        br

        label "Password:"
        input ! type_ "password" ! name "password"
        br

        input ! type_ "submit"

loginTemplate :: Html
loginTemplate = do
    h1 ! class_ "label-green" $ "Login"
    p $ "Enter your username and password below:"
    loginForm Nothing

loginFailTemplate :: Username -> Maybe Password -> Html
loginFailTemplate un Nothing = do
    h1 ! class_ "label-red" $ "Login - No password"
    p $ "Please enter your password below:"
    loginForm (Just un)

loginFailTemplate un (Just _) = do
    h1 ! class_ "label-red" $ "Login - Incorrect password"
    p $ "Please enter your correct password below:"
    loginForm (Just un)


--------------------------------------------------------------------------------
-- Registration templates

regForm :: Maybe Username -> Html
regForm un = do
    form ! action "/happstack-auth/register" ! method "post" ! enctype "multipart/form-data" $ do

        label "Username:"
        input ! type_ "text"     ! name "username" ! value (toValue $ fromMaybe "" un)
        br

        label "Password:"
        input ! type_ "password" ! name "password"
        br

        input ! type_ "submit"

invalidUsernameTemplate :: Username -> Html
invalidUsernameTemplate un = do
    h1 ! class_ "label-green" $ "Register a new user"
    h2 ! class_ "label-red"   $ "Error: Invalid username/password"
    regForm $ Just un

registerTemplate :: Html
registerTemplate = do
    h1 ! class_ "label-green" $ "Register a new user"
    p "Please enter your username/password below:"
    regForm Nothing

newUserTemplate :: Username -> Html
newUserTemplate un = do
    h1 ! class_ "label-green" $ "Registration complete"
    p $ toHtml $ "Welcome " ++ un ++ "!"


--------------------------------------------------------------------------------
-- Statistic template

statsTemplate :: Int -> [Username] -> Int -> Html
statsTemplate nu ulist ns = do
    h1 ! class_ "label-green" $ "Statistics"
    ul ! id "statistics" $ do
        li $ do
            span ! class_ "desc" $ "Number of users:"
            toHtml $ show nu

        li $ do
            span ! class_ "desc" $ "Number of sessions:"
            toHtml $ show ns

        li $ do
            span ! class_ "desc" $ "Current registered users:"
            ul ! id "usernames" $ mapM_ (\un -> li $ toHtml un) ulist

--------------------------------------------------------------------------------
-- Defaults

defaultTemplate :: Html     -- ^ Header
                -> Html     -- ^ Body
                -> Html
defaultTemplate h b =
    docTypeHtml $ do
        head h
        body b

defaultHeader :: Maybe String       -- ^ Title
              -> Html
defaultHeader maybeTitle = do
    meta ! httpEquiv "Content-Type"  ! content "text/html; charset=UTF-8"
    meta ! name "description"        ! content "A Happstack Authentication Suite"
    meta ! name "keywords"           ! content "happstack-auth, happstack, haskell, web framework, web server"
    link ! href "/happstack-auth/theme.css" ! rel "stylesheet" ! type_ "text/css"
    title . toHtml $ "Happstack-Auth" ++ maybe "" (" - " ++) maybeTitle

defaultBody :: Maybe SessionData
            -> String               -- ^ Current url
            -> Html
            -> Html
defaultBody maybeSession cur cont = do
    div ! id "top" $ do
        div ! id "logo" $ do
            span ! class_ "logo-big" $ "Happstack-Auth"
            br
            span ! class_ "logo-sub" $ "A Happstack Authentication Suite"

        ul ! id "main-menu" $ do
            foldr makeMenu (return ()) [ (["", "/"]         , "Home")
                                       , (["/register"]     , "Register")
                                       , (["/login"]        , "Login")
                                       , (["/logout"]       , "Logout")
                                       , (["/stats"]        , "Statistics")
                                       ]

    div ! id "mainbox" $ do
        -- Session info:
        div ! id "sidebar" $ do

            h3 ! class_ "label-green" $ "Quick Links"
            ul ! class_ "quicklinks" $ do
                li $ a ! href "http://n-sch.de/hdocs/happstack-auth"                $ "API Reference"
                li $ a ! href "http://hackage.haskell.org/package/happstack-auth"   $ "Happstack-Auth @ hackageDB"
                li $ a ! href "http://github.com/mcmaniac/happstack-auth"           $ "Happstack-Auth @ github.com"
                li $ a ! href "http://www.happstack.com"                            $ "Happstack Website"
                li $ a ! href "http://groups.google.com/group/HAppS"                $ "Happstack Mailinglist"

            h3 ! class_ "label-green" $ "Current Session"
            case maybeSession of
                 Nothing -> p "Currently not logged in."
                 Just (SessionData _ un c (_,ua)) -> do

                     p . toHtml $ "Logged in as: " ++ un
                     p $ do "Session timeout:"
                            br
                            span ! class_ "session-info" $ toHtml $ show c
                     p $ do "User agent:"
                            br
                            span ! class_ "session-info" $ toHtml $ maybe "-" ((++ "...") . take 30 . unpack) ua

            h3 ! class_ "label-green" $ "Contact"
            p "Nils Schweinsberg"
            ul ! class_ "quicklinks" $ do
                li $ do "Email: mail @ n-sch.de"
                li "Freenode: McManiaC"

        div ! id "content" $ cont

    div ! id "footer" $
        p "Powered by Happstack: A Haskell Web Framework."

  where
    makeMenu :: ([String], String) -> Html -> Html
    makeMenu (paths@(url:_), name) h
        | name == "Register" && isJust    maybeSession = h
        | name == "Login"    && isJust    maybeSession = h
        | name == "Logout"   && isNothing maybeSession = h
        | or (map (\u -> "/happstack-auth" ++ u == cur) paths) = do
            li ! class_ "current" $ a ! href (toValue $ "/happstack-auth" ++ url) $ toHtml name
            h
        | otherwise = do
            li $ a ! href (toValue $ "/happstack-auth" ++ url) $ toHtml name
            h

    makeMenu _ h = h
