{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module Templates where

import Prelude hiding (head, div, id, span)
import Data.List hiding (head, span)
import Data.Maybe

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
        "Happstack-Auth is a authentication suite for the "
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
        "See \"Quick Links\" on the right sidebar." -- TODO :)



--------------------------------------------------------------------------------
-- Login Status templates

loggedInTemplate :: SessionData -> Html
loggedInTemplate (SessionData _ un) = do
    h1 ! class_ "label-red" $ "Already logged in."
    p $ string $ "You are already logged in as " ++ un ++ "."

loginForm :: Maybe Username -> Html
loginForm un =
    form ! action "/login" ! method "post" ! enctype "multipart/form-data" $ do

        label "Username:"
        input ! type_ "text"     ! name "username" ! value (stringValue $ fromMaybe "" un)
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
    form ! action "/register" ! method "post" ! enctype "multipart/form-data" $ do

        label "Username:"
        input ! type_ "text"     ! name "username" ! value (stringValue $ fromMaybe "" un)
        br

        label "Password:"
        input ! type_ "password" ! name "password"
        br

        input ! type_ "submit"


existsTemplate :: Username -> Html
existsTemplate un = do
    h1 ! class_ "label-green" $ "Register a new user"
    h2 ! class_ "label-red"   $ string $ "Error: User " ++ un ++ " already exists."
    regForm $ Just un

registerTemplate :: Html
registerTemplate = do
    h1 ! class_ "label-green" $ "Register a new user"
    p "Please enter your username/password below:"
    regForm Nothing

newUserTemplate :: Username -> Html
newUserTemplate un = do
    h1 ! class_ "label-green" $ "Registration complete"
    p $ string $ "Welcome " ++ un ++ "!"


--------------------------------------------------------------------------------
-- Statistic template

statsTemplate :: Int -> [Username] -> Int -> Html
statsTemplate nu ulist ns = do
    h1 ! class_ "label-green" $ "Statistics"
    ul ! id "statistics" $ do
        li $ do
            span ! class_ "desc" $ "Number of users:"
            string $ show nu

        li $ do
            span ! class_ "desc" $ "Number of sessions:"
            string $ show ns

        li $ do
            span ! class_ "desc" $ "Current registered users:"
            ul ! id "usernames" $ mapM_ (\un -> li $ string un) ulist

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
    meta ! http_equiv "Content-Type" ! content "text/html; charset=UTF-8"
    meta ! name "description"        ! content "A Happstack Authentication Suite"
    meta ! name "keywords"           ! content "happstack-auth, happstack, haskell, web framework, web server"
    link ! href "/theme.css" ! rel "stylesheet" ! type_ "text/css"
    title . string $ "Happstack-Auth" ++ maybe "" (" - " ++) maybeTitle

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
            foldr makeMenu (return ()) [ (["/"]             , "Home")
                                       , (["/register"]     , "Register")
                                       , (["/login"]        , "Login")
                                       , (["/logout"]       , "Logout")
                                       , (["/stats"]        , "Statistics")
                                       ]

    div ! id "mainbox" $ do
        -- Session info:
        div ! id "sidebar" $ do

            h3 ! class_ "label-green" $ "Current Session"
            case maybeSession of
                 Nothing -> p "Currently not logged in."
                 Just (SessionData _ un) -> do
                     p . string $ "Logged in as: " ++ un
                     -- p . string $ "User ID: " ++ show i

            h3 ! class_ "label-green" $ "Quick Links"
            ul ! id "quicklinks" $ do
                li $ a ! href "http://n-sch.de/hdocs/happstack-auth"      $ "API Reference"
                li $ a ! href "http://github.com/mcmaniac/happstack-auth" $ "Happstack-Auth @ github.com"

        div ! id "content" $ cont

    div ! id "footer" $
        p "Powered by Happstack: A Haskell Web Framework."

  where
    makeMenu (paths@(url:_), name) h
        | name == "Register" && isJust    maybeSession = h
        | name == "Login"    && isJust    maybeSession = h
        | name == "Logout"   && isNothing maybeSession = h
        | name == "Home" = do
            if cur == "/" || null cur then
                li ! class_ "current" $ a ! href "/" $ string name
              else
                li $ a ! href "/" $ string name
            h
        | or (map (`isPrefixOf` cur) paths) = do
            li ! class_ "current" $ a ! href (stringValue url) $ string name
            h
        | otherwise = do
            li $ a ! href (stringValue url) $ string name
            h

    makeMenu _ h = h
