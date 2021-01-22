module Api (module Api, module Api.Definition) where
import Servant.API
import Data.Proxy
import Api.Definition
import Servant.Client.JSaddle

((loginUser :<|> registerUser) :<|>
 (getUser :<|> updateUser) :<|>
 (getProfile :<|> followUser :<|> unfollowUser) :<|>
 (getUserArticleFeed :<|> getFeed :<|> postArticle :<|> getArticle :<|> updateArticle :<|> deleteArticle) :<|>
 (getComments :<|> postComment :<|> deleteComment) :<|>
 (favoriteArticle :<|> unfavoriteArticle) :<|>
 getTags) = client (Proxy @API)
