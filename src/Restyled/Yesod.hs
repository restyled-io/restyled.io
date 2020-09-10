{-# LANGUAGE QuasiQuotes #-}

module Restyled.Yesod
    (
    -- * DB
      getEntity404

    -- * Fields
    , jsonField

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
import Yesod.Auth.OAuth2.GitHubStudents as X
import Yesod.Auth.OAuth2.GitLab as X
import Yesod.Core as X
import Yesod.Form as X
import Yesod.Paginator as X
import Yesod.Persist as X (get404, getBy404)

import Restyled.Prelude

getEntity404
    :: (MonadHandler m, SqlEntity a) => Key a -> SqlPersistT m (Entity a)
getEntity404 k = Entity k <$> get404 k

jsonField
    :: ( Monad m
       , RenderMessage (HandlerSite m) FormMessage
       , FromJSON a
       , ToJSON a
       )
    => Field m a
jsonField = Field
    { fieldParse = parseHelper parseVal
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<textarea :isReq:required="" id="#{theId}" name="#{name}" *{attrs}>#{showVal val}
|]
    , fieldEnctype = UrlEncoded
    }
  where
    parseVal :: FromJSON a => Text -> Either FormMessage a
    parseVal = first (MsgInvalidEntry . pack) . eitherDecodeStrict . encodeUtf8

    showVal :: ToJSON a => Either e a -> Text
    showVal = either (const "") $ decodeUtf8 . encodeStrict
