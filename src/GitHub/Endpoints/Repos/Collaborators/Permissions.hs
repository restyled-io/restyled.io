module GitHub.Endpoints.Repos.Collaborators.Permissions
  ( CollaboratorPermission (..)
  , CollaboratorPermissions (..)
  , collaboratorCanRead
  , collaboratorPermissions
  -- , collaboratorPermissions'
  , collaboratorPermissionsR
  ) where

import Prelude

import Data.Aeson
import GitHub.Data
import GitHub.Request

-- data CollaboratorPermission
--     = CollaboratorPermissionAdmin
--     | CollaboratorPermissionRead
--     | CollaboratorPermissionWrite
--     | CollaboratorPermissionNone

-- instance FromJSON CollaboratorPermission where
--     parseJSON = withText "permission" $ \case
--         "admin" -> pure CollaboratorPermissionAdmin
--         "read" -> pure CollaboratorPermissionRead
--         "write" -> pure CollaboratorPermissionWrite
--         "none" -> pure CollaboratorPermissionNone
--         x -> fail $ "Invalid permissions value " <> unpack x

newtype CollaboratorPermissions = CollaboratorPermissions
  { collaboratorPermissionsPermission :: CollaboratorPermission
  }

instance FromJSON CollaboratorPermissions where
  parseJSON = withObject "Permissions" $
    \o -> CollaboratorPermissions <$> o .: "permission"

collaboratorCanRead :: CollaboratorPermissions -> Bool
collaboratorCanRead = permissionCanRead . collaboratorPermissionsPermission
 where
  permissionCanRead = \case
    CollaboratorPermissionAdmin -> True
    CollaboratorPermissionRead -> True
    CollaboratorPermissionWrite -> True
    CollaboratorPermissionNone -> False

collaboratorPermissions
  :: AuthMethod am
  => am
  -> Name Owner
  -> Name Repo
  -> Name User
  -> IO (Either Error CollaboratorPermissions)
collaboratorPermissions auth owner repo user =
  executeRequest auth $ collaboratorPermissionsR owner repo user

-- collaboratorPermissions'
--     :: Name Owner
--     -> Name Repo
--     -> Name User
--     -> IO (Either Error CollaboratorPermissions)
-- collaboratorPermissions' owner repo user =
--     executeRequest' $ collaboratorPermissionsR owner repo user

collaboratorPermissionsR
  :: Name Owner -> Name Repo -> Name User -> Request k CollaboratorPermissions
collaboratorPermissionsR owner repo user =
  query
    [ "repos"
    , toPathPart owner
    , toPathPart repo
    , "collaborators"
    , toPathPart user
    , "permission"
    ]
    []
