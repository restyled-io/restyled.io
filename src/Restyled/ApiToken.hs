{-# LANGUAGE QuasiQuotes #-}

module Restyled.ApiToken
    ( ApiTokenRaw
    , apiTokenRaw
    , createApiToken
    , getUserIdByApiToken
    ) where

import Restyled.Prelude

import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.Persist.Sql (Single(..))
import Database.Persist.Sql.Raw.QQ (executeQQ, sqlQQ)
import Network.HTTP.Types.Header (hAuthorization)
import Restyled.Models
import Restyled.Yesod (MonadHandler, lookupGetParam, lookupHeader)

newtype ApiTokenRaw = ApiTokenRaw
    { apiTokenRaw :: Text
    }

createApiToken :: MonadIO m => UserId -> Text -> SqlPersistT m ApiTokenRaw
createApiToken userId description = do
    raw <- UUID.toText <$> liftIO UUID.nextRandom

    [executeQQ|
        INSERT INTO ^{ApiToken}
            ( @{ApiTokenUser}
            , @{ApiTokenHashed}
            , @{ApiTokenDescription}
            )
        VALUES
            ( #{userId}
            , crypt(#{raw}, gen_salt('bf'))
            , #{description}
            )
    |]

    pure ApiTokenRaw { apiTokenRaw = raw }

getUserIdByApiToken :: MonadHandler m => SqlPersistT m (Maybe UserId)
getUserIdByApiToken = runMaybeT $ do
    ApiTokenRaw {..} <- getApiTokenHeader <|> getApiTokenQuery

    unSingleOne [sqlQQ|
        UPDATE ^{ApiToken}
        SET @{ApiTokenLastUsedAt} = CURRENT_TIMESTAMP
        WHERE @{ApiTokenHashed} = crypt(#{apiTokenRaw}, @{ApiTokenHashed})
        RETURNING @{ApiTokenUser}
    |]

getApiTokenQuery :: MonadHandler m => MaybeT m ApiTokenRaw
getApiTokenQuery = do
    raw <- MaybeT $ lookupGetParam "token"
    pure ApiTokenRaw { apiTokenRaw = raw }

getApiTokenHeader :: MonadHandler m => MaybeT m ApiTokenRaw
getApiTokenHeader = do
    bs <- MaybeT $ lookupHeader hAuthorization
    raw <- hoistMaybe $ T.stripPrefix "token " $ decodeUtf8 bs
    pure ApiTokenRaw { apiTokenRaw = raw }

unSingleOne :: Functor f => f [Single a] -> MaybeT f a
unSingleOne = fmap unSingle . MaybeT . fmap listToMaybe
