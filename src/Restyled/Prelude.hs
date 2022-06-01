{-# LANGUAGE ConstraintKinds #-}

module Restyled.Prelude
    ( module X

    -- * Time
    , getCurrentTime

    -- * List
    , headMaybe

    -- * Bifuctor
    , firstM
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
    ) where

import Relude as X hiding (First(..), Last(..), get)

import Blammo.Logging as X
import Control.Error.Util as X (exceptT, hush, hushT, note, noteT)
import Control.Monad.Except as X
import Control.Monad.Extra as X (fromMaybeM, partitionM)
import Control.Monad.IO.Unlift as X (MonadUnliftIO(..))
import Control.Monad.Reader as X
import Control.Monad.Trans.Maybe as X
import Data.Aeson as X hiding (Key, One, Result(..))
import Data.Aeson.Casing as X
import Data.Bitraversable as X (bimapM)
import Data.Char as X (isSpace, toLower)
import Data.Functor.Syntax as X ((<$$>))
import Data.Hashable as X (Hashable(..))
import Data.List as X (partition)
import Data.List.Extra as X (nubOrd)
import Data.Proxy as X
import Data.Semigroup as X (First(..), Last(..))
import Data.Text as X (pack, unpack)
import Data.Time as X hiding (getCurrentTime)
import Data.Traversable as X (for)
import Database.Persist as X
import Database.Persist.JSONB as X
import Database.Persist.Sql as X (SqlBackend, SqlPersistT)
import Lens.Micro as X (Lens', lens, over)
import Lens.Micro.Mtl as X (view)
import UnliftIO.Concurrent as X (threadDelay)
import UnliftIO.Exception as X
    (catchAny, finally, handle, handleAny, handleJust, throwIO)
import Web.PathPieces as X

-- TODO: put these above/using this prelude
import SVCS.GitHub as X
import SVCS.Names as X
import SVCS.Payload as X

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import Formatting (Format, format, (%))
import qualified Formatting.Formatters as Formatters

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Time.getCurrentTime

headMaybe :: [a] -> Maybe a
headMaybe = fmap NE.head . NE.nonEmpty

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
