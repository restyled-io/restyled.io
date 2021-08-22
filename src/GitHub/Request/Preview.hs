module GitHub.Request.Preview
    ( PreviewRequest
    , MachineManPreviewJson
    ) where

import Prelude

import Data.Aeson
import Data.Tagged (Tagged(..))
import GitHub.Data
import GitHub.Request

-- brittany-disable-next-binding

-- | @'Request'@-like synonym for using @machine-man-preview+json@
type PreviewRequest k a = GenRequest ('MtPreview MachineManPreviewJson) k a

-- | For use in the @'MtPreview'@ @'MediaType'@ itself
data MachineManPreviewJson

-- | Sets @Accept: application/vnd.github.machine-man-preview+json@
--
-- Unclear if this impacts @POST@ bodies, but it seem to work.
--
instance PreviewAccept MachineManPreviewJson where
    previewContentType = "application/vnd.github.machine-man-preview+json"

-- | Use @'parseResponseJSON'@
instance FromJSON a => PreviewParseResponse MachineManPreviewJson a where
    previewParseResponse _req = Tagged . parseResponseJSON
