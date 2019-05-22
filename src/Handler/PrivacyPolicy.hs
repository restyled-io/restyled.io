module Handler.PrivacyPolicy
    ( getPrivacyPolicyR
    )
where

import Import

import Foundation
import Yesod

getPrivacyPolicyR :: Handler Html
getPrivacyPolicyR = redirect @_ @Text
    "https://github.com/restyled-io/restyled.io/wiki/Privacy-Policy"
