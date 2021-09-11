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
        [ "SqlError"
        , "\n  State: " <> displayBytesUtf8 sqlState
        , "\n  ExecStatus: " <> displayShow sqlExecStatus
        , "\n  Message: " <> displayBytesUtf8 sqlErrorMsg
        , "\n  Detail: " <> displayBytesUtf8 sqlErrorDetail
        , "\n  Hint: " <> displayBytesUtf8 sqlErrorHint
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
