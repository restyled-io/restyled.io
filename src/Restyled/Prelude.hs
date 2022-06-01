{-# LANGUAGE ConstraintKinds #-}

module Restyled.Prelude
    (
    -- * Bifuctor
      firstM
    , secondM

    -- * Persist
    , SqlEntity
    , selectFirstT
    , getEntityT
    , getByT
    , getT

    -- * Formatting
    , pluralize
    , pluralizeWith
    , percent

    -- * Text
    , decodeUtf8

    -- * Re-exports
    , module X
    ) where

import RIO as X hiding
    ( Handler
    , LogLevel(..)
    , LogSource
    , logDebug
    , logDebugS
    , logError
    , logErrorS
    , logInfo
    , logInfoS
    , logOther
    , logOtherS
    , logWarn
    , logWarnS
    )

import Control.Error.Util as X
    (exceptT, hoistMaybe, hush, hushT, note, noteT, (??))
import Control.Monad.Except as X
import Control.Monad.Extra as X (fromMaybeM, partitionM)
import Control.Monad.Logger.Aeson as X
import Control.Monad.Trans.Maybe as X
import Data.Aeson as X hiding (Key, Result(..))
import Data.Aeson.Casing as X
import Data.Bitraversable as X (bimapM)
import Data.Char as X (isSpace, toLower)
import Data.Functor.Syntax as X ((<$$>))
import Data.List.Extra as X (sortOn)
import Data.Proxy as X
import Data.Text as X (pack, unpack)
import Database.Persist as X
import Database.Persist.JSONB as X
import Database.Persist.Sql as X (SqlBackend)
import RIO.AWS as X
import RIO.DB as X
import RIO.List as X (headMaybe, partition)
import RIO.Process as X
import RIO.Redis as X
import RIO.Time as X
import SVCS.GitHub as X
import SVCS.Names as X
import SVCS.Payload as X
import Web.PathPieces as X

import qualified Data.Text.Lazy as TL
import Formatting (Format, format, (%))
import qualified Formatting.Formatters as Formatters

firstM :: (Bitraversable t, Applicative f) => (a -> f c) -> t a b -> f (t c b)
firstM f = bimapM f pure

secondM :: (Bitraversable t, Applicative f) => (b -> f c) -> t a b -> f (t a c)
secondM = bimapM pure

type SqlEntity a = (PersistEntity a, PersistEntityBackend a ~ SqlBackend)

selectFirstT
    :: (MonadIO m, SqlEntity a)
    => [Filter a]
    -> [SelectOpt a]
    -> MaybeT (SqlPersistT m) (Entity a)
selectFirstT x = MaybeT . selectFirst x

getEntityT
    :: (MonadIO m, SqlEntity a) => Key a -> MaybeT (SqlPersistT m) (Entity a)
getEntityT = MaybeT . getEntity

getByT
    :: (MonadIO m, SqlEntity a) => Unique a -> MaybeT (SqlPersistT m) (Entity a)
getByT = MaybeT . getBy

getT :: (MonadIO m, SqlEntity a) => Key a -> MaybeT (SqlPersistT m) a
getT = MaybeT . get

pluralize
    :: TL.Text -- ^ Singular
    -> TL.Text -- ^ Plural
    -> Int -- ^ Amount
    -> TL.Text
pluralize = pluralizeWith Formatters.int

pluralizeWith
    :: (Num a, Eq a)
    => Format (a -> TL.Text) (t1 -> t1 -> t2)
    -> TL.Text
    -> TL.Text
    -> t1
    -> t2
pluralizeWith f s p n = format (f % " " % Formatters.plural s p) n n

percent :: (Integral n1, Integral n2) => n1 -> n2 -> TL.Text
percent n1 n2 = format (Formatters.fixed 2 % "%") p
    where p = fromIntegral @_ @Double n1 / fromIntegral n2 * 100

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode
