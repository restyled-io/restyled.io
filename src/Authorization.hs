{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Authorization
    ( requireRepositoryAccess
    , requireRepositoriesAccess
    ) where

import Import

import Database.Persist.Sql (SqlReadT)

-- | Require the current user has access to the given @'Repo'@
--
-- Right now this simply asserts it's public. Some day we may support mirroring
-- GitHub permissions, granting read to admins, etc.
--
requireRepositoryAccess :: MonadHandler m => Repo -> SqlReadT m ()
requireRepositoryAccess Repo{..} | not repoIsPrivate = pure ()
requireRepositoryAccess _ = notFound -- Don't leak existence

-- | Run @'requireRepositoryAccess'@ on all @'Repo'@s in the list
requireRepositoriesAccess :: MonadHandler m => [Entity Repo] -> SqlReadT m ()
requireRepositoriesAccess = traverse_ $ requireRepositoryAccess . entityVal
