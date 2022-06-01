-- | An identifier for the current Transaction, tracked in the 'Request'
module Restyled.Tracing.TransactionId
    ( TransactionId
    , HasTransactionId(..)
    , newTransactionId
    ) where

import Restyled.Prelude

import Data.CaseInsensitive (CI)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Network.Wai (Request(..), Response)
import Yesod.Core.Types (HandlerData)
import Yesod.Core.Types.Lens

newtype TransactionId = TransactionId
    { unTransactionId :: UUID
    }
    deriving newtype (Eq, Hashable)

class HasTransactionId env where
    transactionIdL :: Lens' env (Maybe TransactionId)

instance HasTransactionId Request where
    transactionIdL = headersL . headerL hTransactionId . convertL

instance HasTransactionId Response where
    transactionIdL = headersL . headerL hTransactionId . convertL

instance HasTransactionId (HandlerData child site) where
    transactionIdL = requestL . waiRequestL . transactionIdL

newTransactionId :: MonadIO m => m TransactionId
newTransactionId = TransactionId <$> liftIO UUID.nextRandom

encodeTransactionId :: TransactionId -> ByteString
encodeTransactionId = UUID.toASCIIBytes . unTransactionId

decodeTransactionId :: ByteString -> Maybe TransactionId
decodeTransactionId = fmap TransactionId . UUID.fromASCIIBytes

convertL :: Lens' (Maybe ByteString) (Maybe TransactionId)
convertL = lens getter setter
  where
    getter mbs = decodeTransactionId =<< mbs
    setter mbs = maybe mbs (Just . encodeTransactionId)

hTransactionId :: CI ByteString
hTransactionId = "X-Transaction-Id"
