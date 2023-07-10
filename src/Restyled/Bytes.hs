-- | Models the @--memory=<number>[suffix]@ argument to @docker run@
module Restyled.Bytes
  ( Bytes (..)
  , textToBytes
  , bytesToText
  , Suffix (..)
  , textToSuffix
  , suffixToText
  ) where

import Restyled.Prelude

import qualified Data.Char as Char
import qualified Data.Text as T
import Database.Persist.Sql (PersistFieldSql (..))
import Test.QuickCheck

data Bytes = Bytes
  { bytesNumber :: Natural
  , bytesSuffix :: Maybe Suffix
  }
  deriving stock (Eq, Show)

instance Arbitrary Bytes where
  arbitrary =
    Bytes <$> (fromIntegral @Int . getPositive <$> arbitrary) <*> arbitrary

instance FromJSON Bytes where
  parseJSON = withText "Bytes" $ either fail pure . textToBytes

instance ToJSON Bytes where
  toJSON = toJSON . bytesToText
  toEncoding = toEncoding . bytesToText

instance PersistField Bytes where
  toPersistValue = toPersistValue . bytesToText
  fromPersistValue = first pack . textToBytes <=< fromPersistValue

instance PersistFieldSql Bytes where
  sqlType _ = sqlType $ Proxy @Text

textToBytes :: Text -> Either String Bytes
textToBytes x =
  Bytes
    <$> first unpack (readEither $ unpack number)
    <*> traverse
      textToSuffix
      (guarded (not . T.null) suffix)
 where
  (number, suffix) = T.span ((||) <$> (== '-') <*> Char.isDigit) x

bytesToText :: Bytes -> Text
bytesToText Bytes {..} =
  pack (show bytesNumber) <> maybe "" suffixToText bytesSuffix

data Suffix = B | K | M | G
  deriving stock (Eq, Show, Bounded, Enum)

instance Arbitrary Suffix where
  arbitrary = arbitraryBoundedEnum

textToSuffix :: Text -> Either String Suffix
textToSuffix = \case
  "b" -> Right B
  "k" -> Right K
  "m" -> Right M
  "g" -> Right G
  x ->
    Left $ "Invalid suffix " <> unpack x <> ", must be one of b, k, m, or g"

suffixToText :: Suffix -> Text
suffixToText = \case
  B -> "b"
  K -> "k"
  M -> "m"
  G -> "g"
