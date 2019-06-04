module GitHub.Endpoints.Installations.AccessTokens
    ( accessTokenFor
    , accessTokenForR
    )
where

import Prelude

import GitHub.Data
import GitHub.Data.AccessTokens
import GitHub.Data.Installations
import GitHub.Request
import GitHub.Request.Preview

-- | Get an @'AccessToken'@ for an @'Installation'@
--
-- <https://developer.github.com/v3/apps/#create-a-new-installation-token>
--
accessTokenFor
    :: AuthMethod am => am -> Id Installation -> IO (Either Error AccessToken)
accessTokenFor auth installationId =
    executeRequest auth $ accessTokenForR installationId

accessTokenForR :: Id Installation -> PreviewRequest 'RW AccessToken
accessTokenForR installationId = Command
    Post
    ["installations", toPathPart installationId, "access_tokens"]
    mempty
