{-# LANGUAGE LambdaCase #-}

-- | JSON error representation used in API responses.
module Restyled.ApiError
    ( ApiError(..)
    , sendApiError
    )
where

import Restyled.Prelude

import Network.HTTP.Types.Status
import Restyled.Yesod

data ApiError
    = ApiError Text -- ^ Generic message
    | ApiErrorNotFound Bool -- ^ Was the user logged in?

data ApiErrorResponse = ApiErrorResponse
    { aerName :: Text
    , aerStatus :: Status
    , aerMessage :: Text
    }

apiErrorResponse :: ApiError -> ApiErrorResponse
apiErrorResponse = \case
    ApiError msg -> ApiErrorResponse
        { aerName = "InternalError"
        , aerStatus = status500
        , aerMessage = msg
        }
    ApiErrorNotFound isLoggedIn -> ApiErrorResponse
        { aerName = "NotFound"
        , aerStatus = status404
        , aerMessage = "Page not found"
            <> if isLoggedIn then "" else " (you may need to log in)" <> "."
        }

instance ToJSON ApiErrorResponse where
    toJSON ApiErrorResponse{..} = object
        [ "error" .= aerName
        , "message" .= aerMessage
        ]

-- | Immediately send a response for the given @'ApiError'@
--
-- This could be @m a@ (since it uses the exception-based @'sendStatusJSON'@),
-- but we type it @m 'Value'@ for annotation-free use with @'provideRep'@.
--
sendApiError :: MonadHandler m => ApiError -> m Value
sendApiError apiError = sendStatusJSON (aerStatus errorResponse) errorResponse
    where errorResponse = apiErrorResponse apiError
