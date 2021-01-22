{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DerivingStrategies, DeriveAnyClass #-}
module Api.Definition where
import Servant.API
import Data.Aeson
import GHC.Generics (Generic)
import qualified Api.Types as T
import Miso.String
import Servant.Client.Core.Auth
import qualified Servant.Client.Core as S
import qualified Servant.Client.Core.Request as Req

-- | Aside from renaming Text to MisoString and changing the types of the auth
-- tokens to be opaque strings, this file is unchanged from the server.

type instance AuthClientData (AuthProtect "JWT") = MisoString
type instance AuthClientData (AuthProtect "maybe-JWT") = Maybe MisoString

-- | BE CAREFUL THIS IS NONSTANDARD
-- Virtually everyone sends the token with the "Bearer " schema
-- rather than the "Token " schema. This is simply what RealWorld requires
-- and so we do it here. You should switch to "Bearer " in your own app.
authReq :: MisoString -> S.Request -> S.Request
authReq token = Req.addHeader "Authorization" ("Token " <> token)

authMaybeReq :: Maybe MisoString -> S.Request -> S.Request
authMaybeReq (Just token) = Req.addHeader "Authorization" ("Token " <> token)
authMaybeReq _ = Req.addHeader "Authorization" ("MEOW" :: MisoString)

-- * Wrappers

newtype User a = User { user :: a }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype Profile a = Profile { profile :: a }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype Article a = Article { article :: a }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data Articles = Articles { articles :: [T.Article]
                         , articlesCount :: Int }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype Comment a = Comment { comment :: a }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype Comments a = Comments { comments :: [a] }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype Body = Body { body :: [MisoString] }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

newtype GenericError = GenericError { errors :: Body }
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

type UsersApi =  "login" :> ReqBody '[JSON] (User T.LoginUser) :> Post '[JSON] (User T.User)
            :<|> ReqBody '[JSON] (User T.NewUser) :> Post '[JSON] (User T.User)

type UserApi =  AuthProtect "JWT" :> Get '[JSON] (User T.User)
           :<|> AuthProtect "JWT" :> ReqBody '[JSON] (User T.UpdateUser) :> Put '[JSON] (User T.User)

type ProfilesApi =  Capture "username" T.Username :> AuthProtect "maybe-JWT" :> Get '[JSON] (Profile T.Profile)
               :<|> Capture "username" T.Username :> AuthProtect "JWT" :> "follow" :> Post '[JSON] (Profile T.Profile)
               :<|> Capture "username" T.Username :> AuthProtect "JWT" :> "follow" :> Delete '[JSON] (Profile T.Profile)

type ArticlesApi =
       "feed" :> AuthProtect "JWT" :> QueryParam "limit" Int :> QueryParam "offset" Int :> Get '[JSON] Articles
  :<|> AuthProtect "maybe-JWT" :> QueryParam "tag" MisoString
                               :> QueryParam "author" T.Username
                               :> QueryParam "favorited" T.Username
                               :> QueryParam "limit" Int
                               :> QueryParam "offset" Int :> Get '[JSON] Articles
  :<|> AuthProtect "JWT" :> ReqBody '[JSON] (Article T.NewArticle)                                 :> Post '[JSON] (Article T.Article)
  :<|> AuthProtect "JWT" :> Capture "slug" MisoString                                              :> Get '[JSON] (Article T.Article)
  :<|> AuthProtect "JWT" :> Capture "slug" MisoString :> ReqBody '[JSON] (Article T.UpdateArticle) :> Put '[JSON] (Article T.Article)
  :<|> AuthProtect "JWT" :> Capture "slug" MisoString                                              :> Delete '[JSON] Value

type CommentsApi =
       Capture "slug" MisoString :> "comments" :> AuthProtect "maybe-JWT"                                           :> Get  '[JSON] (Comments T.Comment)
  :<|> Capture "slug" MisoString :> "comments" :> AuthProtect "JWT"       :> ReqBody '[JSON] (Comment T.NewComment) :> Post '[JSON] (Comment T.Comment)
  :<|> Capture "slug" MisoString :> "comments" :> AuthProtect "JWT"       :> Capture "id" Int                       :> Delete '[JSON] Value

type FavoritesApi =
        Capture "slug" MisoString :> "favorite" :> AuthProtect "JWT" :> Post '[JSON] (Article T.Article)
   :<|> Capture "slug" MisoString :> "favorite" :> AuthProtect "JWT" :> Delete '[JSON] (Article T.Article)

type TagsApi = Get '[JSON] T.Tags

-- * API

type API = "api" :> ( "users" :> UsersApi :<|>
                      "user"  :> UserApi :<|>
                      "profiles" :> ProfilesApi :<|>
                      "articles" :> ArticlesApi :<|>
                      "articles" :> CommentsApi :<|>
                      "articles" :> FavoritesApi :<|>
                      "tags" :> TagsApi )
