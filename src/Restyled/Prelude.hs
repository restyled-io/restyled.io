module Restyled.Prelude
    (
    -- * Errors
      fromJustNoteM
    , fromLeftM
    , untryIO

    -- * Persist
    , overEntity
    , replaceEntity
    , selectFirstT
    , getT
    , getEntityT
    , getByT

    -- * ExceptT
    , bimapMExceptT

    -- * IO
    , setLineBuffering

    -- * Formatting
    , pluralize
    , pluralize'

    -- * Re-exports
    , module X
    )
where

import RIO as X hiding (Handler, first, second)

import Control.Error.Util as X
    (exceptT, hoistMaybe, hush, hushT, note, noteT, (??))
import Control.Monad.Except as X
import Control.Monad.Extra as X (fromMaybeM, partitionM)
import Control.Monad.Logger as X
    (logDebugN, logErrorN, logInfoN, logOtherN, logWarnN)
import Control.Monad.Trans.Maybe as X
import Data.Aeson as X hiding (Result(..))
import Data.Aeson.Casing as X
import Data.Bifunctor as X (first, second)
import Data.Bitraversable as X (bimapM)
import Data.ByteString as X (ByteString)
import Data.Char as X (isSpace, toLower)
import Data.Either as X (fromLeft, fromRight)
import Data.Functor.Syntax as X ((<$$>))
import Data.Proxy as X
import Data.Text as X (Text, pack, unpack)
import Data.Time as X
import Database.Persist as X
import Database.Persist.Sql as X (SqlBackend, SqlPersistT)
import RIO.DB as X
import RIO.List as X (headMaybe)
import RIO.Logger as X
import RIO.Process as X
import RIO.Process.Follow as X
import RIO.Redis as X
import SVCS.GitHub as X
import SVCS.Names as X
import SVCS.Payload as X
import System.Exit as X (ExitCode(..))
import Web.PathPieces as X

import qualified Data.Text.Lazy as TL
import Formatting (format, (%))
import Formatting.Formatters (int, plural)
import System.IO (BufferMode(..))

fromJustNoteM :: MonadIO m => String -> Maybe a -> m a
fromJustNoteM msg = fromMaybeM (throwString msg) . pure

fromLeftM :: Monad m => (a -> m b) -> m (Either a b) -> m b
fromLeftM f me = either f pure =<< me

-- | Take an @'IO' ('Either' e a)@ and eliminate via @'throwIO'@
--
-- This effectively reverses @'try'@.
--
untryIO :: (MonadIO m, Exception e) => IO (Either e a) -> m a
untryIO = fromLeftM throwIO . liftIO

overEntity :: Entity a -> (a -> a) -> Entity a
overEntity e f = e { entityVal = f $ entityVal e }

replaceEntity
    :: (MonadIO m, PersistEntity a, PersistEntityBackend a ~ SqlBackend)
    => Entity a
    -> SqlPersistT m ()
replaceEntity (Entity k v) = replace k v

selectFirstT
    :: (MonadIO m, PersistEntity a, PersistEntityBackend a ~ SqlBackend)
    => [Filter a]
    -> [SelectOpt a]
    -> MaybeT (SqlPersistT m) (Entity a)
selectFirstT x = MaybeT . selectFirst x

getT
    :: (MonadIO m, PersistEntity a, PersistEntityBackend a ~ SqlBackend)
    => Key a
    -> MaybeT (SqlPersistT m) a
getT = MaybeT . get

getEntityT
    :: (MonadIO m, PersistEntity a, PersistEntityBackend a ~ SqlBackend)
    => Key a
    -> MaybeT (SqlPersistT m) (Entity a)
getEntityT = MaybeT . getEntity

getByT
    :: (MonadIO m, PersistEntity a, PersistEntityBackend a ~ SqlBackend)
    => Unique a
    -> MaybeT (SqlPersistT m) (Entity a)
getByT = MaybeT . getBy

bimapMExceptT
    :: Monad m => (e -> m f) -> (a -> m b) -> ExceptT e m a -> ExceptT f m b
bimapMExceptT f g (ExceptT m) = ExceptT $ h =<< m
  where
    h (Left e) = Left <$> f e
    h (Right a) = Right <$> g a

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
pluralize s p n = format (int % " " % plural s p) n n

-- | @'pluralize'@ for strict @'Text'@
pluralize'
    :: Text -- ^ Singular
    -> Text -- ^ Plural
    -> Int -- ^ Amount
    -> Text
pluralize' s p = TL.toStrict . pluralize (TL.fromStrict s) (TL.fromStrict p)
