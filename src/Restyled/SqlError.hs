module Restyled.SqlError
    ( SqlError(..)
    , DisplaySqlError(..)
    , displaySqlError
    , handleSqlError
    , SqlState
    , sqlStateQueryCanceled
    , handleSqlErrorState
    ) where

import Restyled.Prelude

import Database.PostgreSQL.Simple (SqlError(..))

-- | Wrapper in order to provide a 'Display' instance
--
-- @
-- 'handleSqlError' ('logError' . 'DisplaySqlError') $ runDB $ insert_ ...
-- @
--
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

-- | Conversion to 'Text', for 'MonadLogger' use
--
-- @
-- 'handleSqlError' ('logErrorN' . 'displaySqlError') $ runDB $ insert_ ...
-- @
--
displaySqlError :: SqlError -> Text
displaySqlError = utf8BuilderToText . display . DisplaySqlError

-- | Type-restricted 'handle'
handleSqlError :: MonadUnliftIO m => (SqlError -> m a) -> m a -> m a
handleSqlError = handle

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
