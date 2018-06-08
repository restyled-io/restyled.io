{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | TODO: Upstream
module Yesod.Paginator.Instances
    ( -- Instances only
    ) where

import Prelude

import Yesod.Paginator

instance Functor Page where
    fmap f page = page
        { pageItems = f <$> pageItems page
        }

instance Foldable Page where
    foldMap f = foldMap f . pageItems

instance Traversable Page where
    traverse f page =
        (\items -> page { pageItems = items })
            <$> traverse f (pageItems page)

instance Functor Pages where
    fmap f pages = pages
        { pagesCurrent = f <$> pagesCurrent pages
        }

instance Foldable Pages where
    foldMap f = foldMap f . pagesCurrent

instance Traversable Pages where
    traverse f pages =
        (\current -> pages { pagesCurrent = current })
            <$> traverse f (pagesCurrent pages)
