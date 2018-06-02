{-# OPTIONS_GHC -fno-warn-orphans #-}

module GitHub.Instances
    (
    -- * Route pieces
      OwnerName
    , RepoName
    )
where

import Prelude

import Data.Proxy
import Data.Text (Text)
import Database.Persist.Sql
import GitHub.Data hiding (Repo(..))
import qualified GitHub.Data as GH
import Text.Blaze (ToMarkup(..))
import Yesod.Core (PathPiece(..))

-- Types necessary to use Names in config/routes
type OwnerName = Name Owner
type RepoName = Name GH.Repo

instance PathPiece (Id a) where
    toPathPiece = toPathPiece . untagId
    fromPathPiece = (mkId Proxy <$>) . fromPathPiece

instance PersistField (Id a) where
    toPersistValue = toPersistValue . untagId
    fromPersistValue = (mkId Proxy <$>) . fromPersistValue

instance PersistFieldSql (Id a) where
    sqlType _ = sqlType (Proxy :: Proxy Int)

instance ToMarkup (Id a) where
    toMarkup = toMarkup . untagId

instance Read (Name a) where
    readsPrec n = map (\(x, s) -> (mkName Proxy x, s)) . readsPrec n

instance PathPiece (Name  a) where
    toPathPiece = toPathPiece . untagName
    fromPathPiece = (mkName Proxy <$>) . fromPathPiece

instance PersistField (Name a) where
    toPersistValue = toPersistValue . untagName
    fromPersistValue = (mkName Proxy <$>) . fromPersistValue

instance PersistFieldSql (Name a) where
    sqlType _ = sqlType (Proxy :: Proxy Text)

instance ToMarkup (Name a) where
    toMarkup = toMarkup . untagName

instance Num (Id a) where
    fromInteger = mkId Proxy . fromInteger

    (+) = errSneakyNum
    (-) = errSneakyNum
    (*) = errSneakyNum
    abs = errSneakyNum
    signum = errSneakyNum

errSneakyNum :: a
errSneakyNum = error "Num instance only present for `fromInteger'"
