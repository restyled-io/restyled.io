module Handler.Admin
    ( getAdminR
    ) where

import Import

getAdminR :: Handler Html
getAdminR = redirect $ AdminP $ AdminReposP AdminReposR
