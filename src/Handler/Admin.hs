-- | Redirect to a good route for Admin homepage
module Handler.Admin
    ( getAdminR
    ) where

import Import

getAdminR :: Handler Html
getAdminR = redirect $ AdminP $ AdminJobsP AdminJobsR
