module Restyled.Prelude
    (
    -- * Errors
      fromJustNoteM
    , fromLeftM

    -- * Persist
    , overEntity
    , replaceEntity

    -- * ExceptT
    , bimapMExceptT

    -- * IO
    , setLineBuffering

    -- * Formatting
    , pluralize

    -- * Re-exports
    , module X
    )
where


import RIO as X hiding (first, second)

import Control.Error.Util as X
    (exceptT, hoistMaybe, hush, hushT, note, noteT, (??))
import Control.Monad.Except as X
import Control.Monad.Extra as X (fromMaybeM)
import Control.Monad.Logger as X
    (logDebugN, logErrorN, logInfoN, logOtherN, logWarnN)
import Control.Monad.Trans.Maybe as X
import Data.Aeson as X hiding (Result(..))
import Data.Aeson.Casing as X
import Data.Bifunctor as X (first, second)
import Data.ByteString as X (ByteString)
import Data.Char as X (isSpace, toLower)
import Data.Either as X (fromLeft, fromRight)
import Data.Functor.Syntax as X ((<$$>))
import Data.Proxy as X
import Data.Text as X (Text, pack, unpack)
import Data.Time as X
import Database.Persist as X
import Database.Persist.Sql as X (SqlBackend, SqlPersistT)
import GitHub.Data as X (toPathPart)
import RIO.DB as X
import RIO.Handler as X
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

overEntity :: Entity a -> (a -> a) -> Entity a
overEntity e f = e { entityVal = f $ entityVal e }

replaceEntity
    :: (MonadIO m, PersistEntity a, PersistEntityBackend a ~ SqlBackend)
    => Entity a
    -> SqlPersistT m ()
replaceEntity (Entity k v) = replace k v

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

pluralize :: TL.Text -> TL.Text -> Int -> TL.Text
pluralize s p n = format (int % " " % plural s p) n n
