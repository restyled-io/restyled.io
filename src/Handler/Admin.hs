module Handler.Admin
    ( getAdminR
    )
where

import Import

import Foundation
import Yesod

getAdminR :: Handler Html
getAdminR = redirect $ AdminP $ AdminJobsP AdminJobsR
