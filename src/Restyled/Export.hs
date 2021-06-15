module Restyled.Export
    ( ExportOptions
    , exportOptions
    , runExport
    ) where

import Restyled.Prelude

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import Options.Applicative
import Restyled.Export.Customers
import Restyled.Settings

data ExportOptions = ExportCustomers

-- brittany-disable-next-binding

exportOptions :: Parser ExportOptions
exportOptions = subparser
    $ command "customers" (info (pure ExportCustomers) mempty)

runExport
    :: (HasLogFunc env, HasSettings env, HasSqlPool env, HasRedis env)
    => ExportOptions
    -> RIO env ()
runExport ExportCustomers = putCsv =<< fetchCustomers

putCsv :: (MonadIO m, Csv.DefaultOrdered a, Csv.ToNamedRecord a) => [a] -> m ()
putCsv = liftIO . BSL.putStr . Csv.encodeDefaultOrderedByNameWith options
    where options = Csv.defaultEncodeOptions { Csv.encUseCrLf = False }
