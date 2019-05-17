module Backend.Import
    ( module X

    , assertJust
    , encodeStrict
    )
where

import RIO as X hiding (timeout)

import Control.Error.Util as X (hush, hushT, note, noteT, (??))
import Control.Monad.Except as X
    (ExceptT(..), liftEither, runExceptT, throwError, withExceptT)
import Control.Monad.Trans.Maybe as X
import Data.Aeson as X
import Data.Aeson.Casing as X
import Data.ByteString as X (ByteString)
import Data.Proxy as X
import Data.Text as X (Text, pack, unpack)
import Data.Time as X
import Database.Persist as X
import Database.Persist.Sql as X (SqlPersistT)
import RIO.DB as X
import RIO.Redis as X
import System.Exit as X (ExitCode(..))
import Web.PathPieces as X

-- * Lowest-level Yesod app modules we use
import Import.NoFoundation as X (fromMaybeM, overEntity, (<$$>))
import Model as X
import Model.Job as X
import Model.Repo as X
import Model.RestyleMachine as X
import Model.User as X
import Settings as X

import Data.ByteString.Lazy (toStrict)

assertJust :: (HasCallStack, MonadIO m) => String -> Maybe a -> m a
assertJust msg = fromMaybeM (throwString $ "Failed assertion: " <> msg)

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = toStrict . encode
