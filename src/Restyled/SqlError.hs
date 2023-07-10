module Restyled.SqlError
  ( SqlError (..)
  , SqlState
  , sqlStateQueryCanceled
  , handleSqlErrorState
  ) where

import Restyled.Prelude

import Database.PostgreSQL.Simple (SqlError (..))

-- | Encapsulation of known States to handle by
--
-- See 'handleSqlErrorState'
newtype SqlState = SqlState ByteString

-- | Query was canceled, likely due to @statement_timeout@
sqlStateQueryCanceled :: SqlState
sqlStateQueryCanceled = SqlState "57014"

-- | 'handleSqlError', only if specific states
handleSqlErrorState
  :: MonadUnliftIO m => SqlState -> (SqlError -> m a) -> m a -> m a
handleSqlErrorState (SqlState x) =
  handleJust $ \ex -> ex <$ guard (sqlState ex == x)
