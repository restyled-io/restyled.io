module Import.NoFoundation
    ( module Import
    , module Import.NoFoundation
    )
where

import ClassyPrelude.Yesod as Import hiding (Proxy)
import Control.Error.Util as Import (hush, hushT, note, noteT, (??))
import Control.Monad.Except as Import (ExceptT, liftEither, runExceptT)
import Control.Monad.Logger as Import
import Control.Monad.Trans.Maybe as Import
import Control.Monad.Extra as Import (fromMaybeM)
import Data.Proxy as Import
import Data.Time as Import
import Model as Import
import Model.Job as Import
import Model.Repo as Import
import Model.User as Import
import Settings as Import
import Settings.StaticFiles as Import
import Data.Functor.Syntax as Import ((<$$>))

import qualified Data.Text.Lazy as TL
import Formatting (format, (%))
import Formatting.Formatters (int, plural)
import RIO (RIO, runRIO)

runHandlerRIO :: RIO env a -> HandlerFor env a
runHandlerRIO f = do
    app <- getYesod
    runRIO app f

pluralize :: TL.Text -> TL.Text -> Int -> TL.Text
pluralize s p n = format (int % " " % plural s p) n n
