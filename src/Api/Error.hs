{-# LANGUAGE LambdaCase #-}

-- | JSON error representation used in API responses.
module Api.Error
    ( ApiError(..)
    , sendApiError
    )
where

import Restyled.Prelude

import Network.HTTP.Types.Status
import Yesod.Core (MonadHandler)
import Yesod.Core.Handler (sendStatusJSON)

data ApiError
    = ApiError Text -- ^ Generic message
    | ApiErrorNotFound Bool -- ^ Was the user logged in?

data ErrorResponse = ErrorResponse
    { erName :: Text
    , erStatus :: Status
    , erMessage :: Text
    }

apiErrorResponse :: ApiError -> ErrorResponse
apiErrorResponse = \case
    ApiError msg -> ErrorResponse
        { erName = "InternalError"
        , erStatus = status500
        , erMessage = msg
        }
    ApiErrorNotFound isLoggedIn -> ErrorResponse
        { erName = "NotFound"
        , erStatus = status404
        , erMessage = "Page not found"
            <> if isLoggedIn then "" else " (you may need to log in)" <> "."
        }

instance ToJSON ErrorResponse where
    toJSON ErrorResponse{..} = object
        [ "error" .= erName
        , "message" .= erMessage
        ]

-- | Immediately send a response for the given @'ApiError'@
--
-- This could be @m a@ (since it uses the exception-based @'sendStatusJSON'@),
-- but we type it @m 'Value'@ for annotation-free use with @'provideRep'@.
--
sendApiError :: MonadHandler m => ApiError -> m Value
sendApiError apiError = sendStatusJSON (erStatus errorResponse) errorResponse
    where errorResponse = apiErrorResponse apiError
