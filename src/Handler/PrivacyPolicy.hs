{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.PrivacyPolicy
    ( getPrivacyPolicyR
    ) where

import Import

getPrivacyPolicyR :: Handler Html
getPrivacyPolicyR = defaultLayout $ do
    setTitle "Restyled Privacy Policy"
    $(widgetFile "privacy-policy")
