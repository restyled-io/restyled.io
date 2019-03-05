module Handler.PrivacyPolicy
    ( getPrivacyPolicyR
    ) where

import Import

getPrivacyPolicyR :: Handler Html
getPrivacyPolicyR =
    redirect "https://github.com/restyled-io/restyled.io/wiki/Privacy-Policy"
