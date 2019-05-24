module Restyled.Handlers.Admin
    ( getAdminR
    )
where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Yesod

getAdminR :: Handler Html
getAdminR = redirect $ AdminP $ AdminJobsP AdminJobsR
