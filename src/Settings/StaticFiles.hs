module Settings.StaticFiles
    ( staticR
    )
where

import Prelude

import Yesod
import Yesod.Static

staticR :: FilePath -> Route Static
staticR _ = StaticRoute ["x", "y"] []
