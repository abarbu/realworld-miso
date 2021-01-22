{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies, GeneralizedNewtypeDeriving, DuplicateRecordFields, DisambiguateRecordFields #-}
module Api.Types where
import Miso
import Miso.String
import Data.Time.Clock
import Data.Aeson
import GHC.Generics (Generic)
import Servant.API
import Data.JSString.Text

instance FromHttpApiData JSString where
  parseUrlPiece t = case parseUrlPiece t of
                      Right t -> Right $ textToJSString t
                      Left x -> Left x
  parseQueryParam t = case parseUrlPiece t of
                        Right t -> Right $ textToJSString t
                        Left x -> Left x

instance ToHttpApiData JSString where
  toUrlPiece = toUrlPiece . textFromJSString
  toQueryParam = toQueryParam . textFromJSString

-- * Our types

newtype Username = Username { unUsername :: MisoString }
  deriving (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

newtype Password = Password { unPassword :: MisoString }
  deriving (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

newtype Email = Email { unEmail :: MisoString }
  deriving (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData)

-- * API types

data LoginUser = LoginUser { email :: Email
                           , password :: Password }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

data NewUser = NewUser { username :: Username
                       , email :: Email
                       , password :: Password }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

data User = User { email :: Email
                 , username :: Username
                 , bio :: Maybe MisoString
                 , image :: Maybe MisoString
                 , token :: MisoString }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

data UpdateUser = UpdateUser { email :: Email
                             , username :: Username
                             , bio :: Maybe MisoString
                             , image :: Maybe MisoString
                             , password :: Maybe MisoString }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

data Profile = Profile { username :: Username
                       , bio :: Maybe MisoString
                       , image :: MisoString
                       , following :: Bool }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

data Article = Article { slug :: MisoString
                       , title :: MisoString
                       , description :: MisoString
                       , body :: MisoString
                       , tagList :: [MisoString]
                       , createdAt :: UTCTime
                       , updatedAt :: UTCTime
                       , favorited :: Bool
                       , favoritesCount :: Int
                       , author :: Profile }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

data NewArticle = NewArticle { title :: MisoString
                             , description :: MisoString
                             , body :: MisoString
                             , tagList :: [MisoString] }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

data UpdateArticle = UpdateArticle { title :: Maybe MisoString
                                   , description :: Maybe MisoString
                                   , body :: Maybe MisoString
                                   , tagList :: Maybe [MisoString] }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

data NewComment = NewComment { body :: MisoString }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

data Comment = Comment { id :: Int
                       , createdAt :: UTCTime
                       , updatedAt :: UTCTime
                       , body :: MisoString
                       , author :: Profile }
  deriving (Eq, Generic, ToJSON, FromJSON, Show)

newtype Tags = Tags { tags :: [MisoString] }
  deriving (Eq, Generic, Show)
  deriving anyclass (ToJSON, FromJSON)
