{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Revision
    ( getRevisionR
    ) where

import Restyled.Prelude

#ifdef DOCKERIZED
import Data.FileEmbed (embedFile)
#else
import Development.GitRev (gitCommitDate, gitHash)
#endif
import Restyled.Foundation
import Restyled.Yesod

getRevisionR :: Handler TypedContent
getRevisionR = pure . TypedContent typePlain $ toContent appRevision

-- brittany-disable-next-binding
appRevision :: ByteString
appRevision =
#ifdef DOCKERIZED
    $(embedFile "config/revision")
#else
    $(gitHash) <> " - " <> $(gitCommitDate)
#endif
