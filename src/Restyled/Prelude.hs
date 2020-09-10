{-# LANGUAGE ConstraintKinds #-}

module Restyled.Prelude
    (
    -- * Errors
      fromLeftM
    , untryIO

    -- * Persist
    , SqlEntity
    , overEntity
    , replaceEntity
    , selectFirstT
    , getEntityT
    , getByT

    -- * IO
    , setLineBuffering

    -- * Formatting
    , pluralize
    , pluralizeWith

    -- * Text
    , decodeUtf8

    -- * Re-exports
    , module X
    )
where

import RIO as X hiding (Handler)

import Control.Error.Util as X
    (exceptT, hoistMaybe, hush, hushT, note, noteT, (??))
import Control.Monad.Except as X
import Control.Monad.Extra as X (fromMaybeM, partitionM)
import Control.Monad.Logger as X
    (logDebugN, logErrorN, logInfoN, logOtherN, logWarnN)
import Control.Monad.Trans.Maybe as X
import Data.Aeson as X hiding (Result(..))
import Data.Aeson.Casing as X
import Data.Bitraversable as X (bimapM)
import Data.Char as X (isSpace, toLower)
import Data.Either as X (fromLeft, fromRight)
import Data.Functor.Syntax as X ((<$$>))
import Data.List.Extra as X (sortOn)
import Data.Proxy as X
import Data.Text as X (pack, unpack)
import Database.Persist as X
import Database.Persist.JSONB as X
import Database.Persist.Sql as X (SqlBackend)
import RIO.DB as X
import RIO.List as X (headMaybe, partition)
import RIO.Logger as X
import RIO.Process as X
import RIO.Process.Follow as X
import RIO.Redis as X
import RIO.Time as X
import SVCS.GitHub as X
import SVCS.Names as X
import SVCS.Payload as X
import Web.PathPieces as X

import qualified Data.Text.Lazy as TL
import Formatting (Format, format, (%))
import qualified Formatting.Formatters as Formatters

fromLeftM :: Monad m => (a -> m b) -> m (Either a b) -> m b
fromLeftM f me = either f pure =<< me

-- | Take an @'IO' ('Either' e a)@ and eliminate via @'throwIO'@
--
-- This effectively reverses @'try'@.
--
untryIO :: (MonadIO m, Exception e) => IO (Either e a) -> m a
untryIO = fromLeftM throwIO . liftIO

type SqlEntity a = (PersistEntity a, PersistEntityBackend a ~ SqlBackend)

overEntity :: Entity a -> (a -> a) -> Entity a
overEntity e f = e { entityVal = f $ entityVal e }

replaceEntity
    :: (MonadIO m, SqlEntity a) => Entity a -> SqlPersistT m (Entity a)
replaceEntity e@(Entity k v) = e <$ replace k v

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

-- | Set output handles to line buffering
--
-- Required to ensure container logs are visible immediately.
--
setLineBuffering :: MonadIO m => m ()
setLineBuffering = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

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

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode
