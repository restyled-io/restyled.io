{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.Yesod
    (
    -- * Safer defaults
      requireJsonBody

    -- * DB
    , getEntity404

    -- * TypedContent
    , sendResponseCSV

    -- * Re-exports
    , module X
    ) where

import Network.HTTP.Types.Status as X
import Yesod.Auth as X
import Yesod.Auth.Message as X
import Yesod.Auth.OAuth2 as X
import Yesod.Auth.OAuth2.GitHub as X
import Yesod.Auth.OAuth2.GitHubStudents as X
import Yesod.Auth.OAuth2.GitLab as X
import Yesod.Core as X hiding (requireJsonBody)
import Yesod.Form as X
import Yesod.Core.Types.Lens as X
import Yesod.Paginator as X
import Yesod.Persist as X (get404, getBy404)

import Data.Csv (DefaultOrdered(..), ToNamedRecord)
import qualified Data.Csv as Csv
import Data.Csv.Builder (encodeHeaderWith, encodeNamedRecordWith)
import Restyled.Prelude
import Yesod.Core.Types (HandlerData)

instance HasLogFunc env => HasLogFunc (HandlerData child env) where
    logFuncL = envL . siteL . logFuncL

instance HasProcessContext env => HasProcessContext (HandlerData child env) where
    processContextL = envL . siteL . processContextL

requireJsonBody :: (MonadHandler m, FromJSON a) => m a
requireJsonBody = requireCheckJsonBody

getEntity404
    :: (MonadHandler m, SqlEntity a) => Key a -> SqlPersistT m (Entity a)
getEntity404 k = Entity k <$> get404 k

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
