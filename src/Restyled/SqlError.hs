module Restyled.SqlError
    ( SqlError(..)
    , DisplaySqlError(..)
    , SqlState
    , sqlStateQueryCanceled
    , handleSqlErrorState
    ) where

import Restyled.Prelude

import Database.PostgreSQL.Simple (SqlError(..))

-- | Wrapper in order to provide a 'Display' instance
newtype DisplaySqlError = DisplaySqlError SqlError

instance Display DisplaySqlError where
    display (DisplaySqlError SqlError {..}) = mconcat
        [ "SqlError "
        , "{ state=" <> displayBytesUtf8 sqlState
        , ", execStatus=" <> displayShow sqlExecStatus
        , ", message=" <> displayBytesUtf8 sqlErrorMsg
        , ", detail=" <> displayBytesUtf8 sqlErrorDetail
        , ", hint=" <> displayBytesUtf8 sqlErrorHint
        , "}"
        ]

-- | Encapsulation of known States to handle by
--
-- See 'handleSqlErrorState'
--
newtype SqlState = SqlState ByteString

-- | Query was canceled, likely due to @statement_timeout@
sqlStateQueryCanceled :: SqlState
sqlStateQueryCanceled = SqlState "57014"

-- | 'handleSqlError', only if specific states
handleSqlErrorState
    :: MonadUnliftIO m => SqlState -> (SqlError -> m a) -> m a -> m a
handleSqlErrorState (SqlState x) =
    handleJust $ \ex -> ex <$ guard (sqlState ex == x)
