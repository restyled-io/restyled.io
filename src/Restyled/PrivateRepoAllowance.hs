module Restyled.PrivateRepoAllowance
    ( PrivateRepoAllowance(..)
    , fromNumeric
    ) where

import Restyled.Prelude

import Database.Persist.Sql (PersistFieldSql(..))

data PrivateRepoAllowance
    = PrivateRepoAllowanceNone
    | PrivateRepoAllowanceUnlimited
    | PrivateRepoAllowanceLimited Natural
    deriving stock (Eq, Show)

fromNumeric :: Int -> PrivateRepoAllowance
fromNumeric n
    | n == 0 = PrivateRepoAllowanceNone
    | n < 0 = PrivateRepoAllowanceUnlimited
    | otherwise = PrivateRepoAllowanceLimited $ fromIntegral n

toNumeric :: PrivateRepoAllowance -> Int
toNumeric = \case
    PrivateRepoAllowanceNone -> 0
    PrivateRepoAllowanceUnlimited -> -1
    PrivateRepoAllowanceLimited n -> fromIntegral n

instance FromJSON PrivateRepoAllowance where
    parseJSON v = fromNumeric <$> parseJSON v

instance ToJSON PrivateRepoAllowance where
    toJSON = toJSON . toNumeric
    toEncoding = toEncoding . toNumeric

instance PersistField PrivateRepoAllowance where
    toPersistValue = toPersistValue . toNumeric
    fromPersistValue v = fromNumeric <$> fromPersistValue v

instance PersistFieldSql PrivateRepoAllowance where
    sqlType _ = sqlType $ Proxy @Int
