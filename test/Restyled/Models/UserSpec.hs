module Restyled.Models.UserSpec
  ( spec
  ) where

import Restyled.Test

import Restyled.Queues
import Restyled.RestylerImage
import qualified Prelude as Unsafe

data AdminSettings = AdminSettings
  { asAppSettings :: AppSettings
  -- ^ An @'AppSettings'@ with non-empty @'appAdmins'@
  , asSomeAdminEmail :: Text
  -- ^ A random element from the @'appAdmins'@ list
  , asSomeOtherEmail :: Text
  -- ^ A random email /not/ in said list
  }

instance Show AdminSettings where
  show AdminSettings {..} =
    unpack
      $ unlines
        [ "AdminSettings"
        , "  { asAppSettings ="
        , "    AppSettings"
        , "      { appAdmins = " <> show @Text (appAdmins asAppSettings)
        , "      , ..."
        , "      }"
        , "  , asSomeAdminEmail = " <> show @Text asSomeAdminEmail
        , "  , asSomeOtherEmail = " <> show @Text asSomeOtherEmail
        , "  }"
        ]

instance Arbitrary AdminSettings where
  arbitrary = do
    len <- getPositive <$> arbitrary
    idx <- choose (0, len - 1)
    emails <- replicateM len arbitrary
    someOtherEmail <- arbitrary `suchThat` (`notElem` emails)
    pure
      AdminSettings
        { asAppSettings = emptySettings {appAdmins = map pack emails}
        , asSomeAdminEmail = pack $ emails Unsafe.!! idx
        , asSomeOtherEmail = pack someOtherEmail
        }

spec :: Spec
spec = do
  describe "userIsAdmin" $ do
    it "is False without admins"
      $ property
      $ not
      . userIsAdmin emptySettings
      . userWithEmail
      . pack

    it "is False without email" $ property $ \AdminSettings {..} ->
      not $ userIsAdmin asAppSettings emptyUser

    it "is False for non-admins" $ property $ \AdminSettings {..} ->
      not $ userIsAdmin asAppSettings $ userWithEmail asSomeOtherEmail

    it "is True for admins" $ property $ \AdminSettings {..} ->
      userIsAdmin asAppSettings $ userWithEmail asSomeAdminEmail

emptySettings :: AppSettings
emptySettings =
  AppSettings
    { appDatabaseConf = error "unused"
    , appStatementTimeout = error "unused"
    , appRedisConf = error "unused"
    , appRoot = error "unused"
    , appHost = error "unused"
    , appPort = 0
    , appForceSSL = False
    , appLogSettings = error "unused"
    , appCopyright = ""
    , appGitHubAppId = error "unused"
    , appGitHubAppKey = error "unused"
    , appGitHubOAuthKeys = Nothing
    , appGitHubRateLimitToken = error "unused"
    , appGitLabOAuthKeys = Nothing
    , appGitHubStudentsOAuthKeys = Nothing
    , appRestylerImage = restylerImage "restyled/restyler:main"
    , appAdmins = []
    , appAllowDummyAuth = error "unused"
    , appFavicon = error "unused"
    , appMutableStatic = False
    , appStaticDir = error "unused"
    , appStubMarketplaceListing = True
    , appRestyleMachineLocal = True
    , appRestyleMachineJobsMax = 3
    , appRequestTimeout = 30
    , appRestylerLogGroup = ""
    , appRestylerLogStreamPrefix = ""
    , appRestylerQueues = defaultQueues
    }

userWithEmail :: Text -> User
userWithEmail email = emptyUser {userEmail = Just email}

emptyUser :: User
emptyUser =
  User
    { userEmail = Nothing
    , userGithubUserId = Nothing
    , userGithubUsername = Nothing
    , userGitlabUserId = Nothing
    , userGitlabUsername = Nothing
    , userGitlabAccessToken = Nothing
    , userGitlabRefreshToken = Nothing
    , userCredsIdent = ""
    , userCredsPlugin = ""
    }
