module Backend.Import
    ( module X
    , module Backend.Import
    )
where

import RIO as X hiding (timeout)

import Control.Error.Util as X (bimapExceptT, hush, hushT, note, noteT, (??))
import Control.Monad.Except as X
    (ExceptT(..), liftEither, runExceptT, throwError, withExceptT)
import Control.Monad.Extra as X (fromMaybeM)
import Control.Monad.Trans.Maybe as X
import Data.Aeson as X
import Data.Aeson.Casing as X
import Data.ByteString as X (ByteString)
import Data.Functor.Syntax as X ((<$$>))
import Data.Proxy as X
import Data.Text as X (Text, pack, unpack)
import Data.Time as X
import Database.Persist as X
import Database.Persist.Sql as X (SqlPersistT)
import Model as X
import Model.Job as X
import Model.Repo as X
import Model.User as X
import RIO.DB as X
import RIO.Redis as X
import Settings as X
import System.Exit as X (ExitCode(..))
import Web.PathPieces as X

fromJustNoteM :: MonadIO m => String -> Maybe a -> m a
fromJustNoteM msg = fromMaybeM (throwString msg) . pure

fromLeftM :: Monad m => (a -> m b) -> m (Either a b) -> m b
fromLeftM f me = either f pure =<< me

overEntity :: Entity a -> (a -> a) -> Entity a
overEntity e f = e { entityVal = f $ entityVal e }
