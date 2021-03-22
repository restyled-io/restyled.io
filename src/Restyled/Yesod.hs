{-# LANGUAGE QuasiQuotes #-}

module Restyled.Yesod
    (
    -- * DB
      getEntity404

    -- * Fields
    , jsonField

    -- * TypedContent
    , sendResponseCSV

    -- * Re-exports
    , module X
    ) where

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

import Data.Csv (DefaultOrdered(..), ToNamedRecord)
import qualified Data.Csv as Csv
import Data.Csv.Builder (encodeHeaderWith, encodeNamedRecordWith)
import Restyled.Prelude

getEntity404
    :: (MonadHandler m, SqlEntity a) => Key a -> SqlPersistT m (Entity a)
getEntity404 k = Entity k <$> get404 k

jsonField
    :: (Monad m, RenderMessage (HandlerSite m) FormMessage, FromJSON a)
    => (a -> Text)
    -> Field m a
jsonField enc = Field
    { fieldParse = parseHelper parseVal
    , fieldView = \theId name attrs val isReq -> toWidget [hamlet|
$newline never
<textarea :isReq:required="" id="#{theId}" name="#{name}" *{attrs}>#{either id enc val}
|]
    , fieldEnctype = UrlEncoded
    }
  where
    parseVal :: FromJSON a => Text -> Either FormMessage a
    parseVal = first (MsgInvalidEntry . pack) . eitherDecodeStrict . encodeUtf8

sendResponseCSV
    :: forall m t a r
     . (MonadHandler m, Foldable t, DefaultOrdered a, ToNamedRecord a)
    => t a
    -> m r
sendResponseCSV =
    sendResponse . TypedContent typeCsv . flip ContentBuilder Nothing . foldl'
        (\b a -> b <> encodeNamedRecordWith ops hdr a)
        (encodeHeaderWith ops hdr)
  where
    hdr = Csv.headerOrder @a undefined
    ops = Csv.defaultEncodeOptions { Csv.encUseCrLf = False }

typeCsv :: ContentType
typeCsv = "text/csv; charset=utf-8"
