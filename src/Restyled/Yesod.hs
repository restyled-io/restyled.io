module Restyled.Yesod
    (
    -- * Fields
      epochField

    -- * Re-exports
    , module X
    )
where

import Network.HTTP.Types.Status as X
import RIO.Handler as X ()
import Yesod.Auth as X
import Yesod.Auth.Message as X
import Yesod.Auth.OAuth2 as X
import Yesod.Auth.OAuth2.GitHub as X
import Yesod.Auth.OAuth2.GitLab as X
import Yesod.Core as X
import Yesod.Form as X
import Yesod.Paginator as X
import Yesod.Persist as X (get404, getBy404)

import Restyled.Prelude

epochField
    :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m UTCTime
epochField = inputField parseEpoch

parseEpoch :: Text -> Either FormMessage UTCTime
parseEpoch t =
    note errMessage $ parseTimeM True defaultTimeLocale "%s" $ unpack t
  where
    errMessage :: FormMessage
    errMessage =
        MsgInvalidNumber
            $ t
            <> " did not parse as whole seconds since Unix epoch"

inputField
    :: (Monad m, RenderMessage (HandlerSite m) FormMessage)
    => (Text -> Either FormMessage a)
    -> Field m a
inputField p = Field
    { fieldParse = parseHelper p
    , fieldView = error "No view for inputs"
    , fieldEnctype = error "No enctype for inputs"
    }
