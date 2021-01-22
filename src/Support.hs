module Support(module Support, module Routes, module Data.Maybe,
               module Control.Monad.State.Class, module Control.Lens,
               module Misc, module GHC.Records, module G, module Servant.Client.Core.Auth,
               module Control.Applicative, module Control.Monad
              ) where
import Miso as M hiding (at)
import Miso.String as MS hiding(map) 
import Misc
import qualified Api as A
import qualified Api.Types as AT
import Control.Lens
import GHC.Records
import GHC.Generics as G (Generic) 
import Data.Generics.Labels ()
import Servant.Client.Core.Auth
import Data.Maybe
import Routes
import Data.Time.Format
import Control.Monad.State.Class
import Control.Applicative
import Control.Monad

-- int is the page
data FeedSelected = MyFeed Int | UserFeed AT.Username Int | FavoriteFeed AT.Username Int | GlobalFeed Int | TagFeed MisoString Int
  deriving (Show, Eq, Generic)

data ActionSelectFeed = SelectFeed FeedSelected
  deriving (Show)

pageNumber (MyFeed n) = n
pageNumber (FavoriteFeed _ n) = n
pageNumber (GlobalFeed n) = n
pageNumber (TagFeed _ n) = n
pageNumber (UserFeed _ n) = n

setPage (MyFeed _) n =  MyFeed n
setPage (FavoriteFeed s _) n = FavoriteFeed s n
setPage (GlobalFeed _) n = GlobalFeed n
setPage (TagFeed s _) n = TagFeed s n
setPage (UserFeed s _) n = UserFeed s n

updateFeedSelected :: Int -> (Either MisoString A.Articles -> a) -> ActionSelectFeed -> GlobalModel -> Transition (Action' a) m ()
updateFeedSelected articlesPerPage r (SelectFeed f@(MyFeed _)) gm = do
  apiCall gm r
    $ A.getUserArticleFeed (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq)
                        (Just articlesPerPage) (Just $ articlesPerPage * (pageNumber f - 1))
updateFeedSelected articlesPerPage r (SelectFeed f) gm = do
  apiCall gm r
    $ A.getFeed (mkAuthenticatedRequest (gm ^? #modelLoggedInUser . _Just . #token) A.authMaybeReq)
                (case f of
                           UserFeed u _ -> Nothing
                           GlobalFeed _ -> Nothing
                           FavoriteFeed u _ -> Nothing
                           TagFeed t _ -> Just t
                           MyFeed _ -> Nothing)
                (case f of
                           UserFeed u _ -> Just u
                           GlobalFeed _ -> Nothing
                           FavoriteFeed u _ -> Nothing
                           TagFeed _ _ -> Nothing
                           MyFeed _ -> Nothing)
                (case f of
                           UserFeed u _ -> Nothing
                           GlobalFeed _ -> Nothing
                           FavoriteFeed u _ -> Just u
                           TagFeed _ _ -> Nothing
                           MyFeed _ -> Nothing)
                (Just articlesPerPage) (Just $ articlesPerPage * (pageNumber f - 1))

data ActionFollow = Follow MisoString
                  | Unfollow MisoString
  deriving (Show)

updateFollow r (Follow u) gm = do
  apiCall gm r
    $ A.followUser (AT.Username u) (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq)
updateFollow r (Unfollow u) gm = do
  apiCall gm r
    $ A.unfollowUser (AT.Username u) (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq)

data ActionFavorite = FavoriteArticle MisoString
                    | UnFavoriteArticle MisoString
                    | ReceiveFavorite (Either MisoString (A.Article AT.Article))
  deriving (Show)

replaceArticle :: AT.Article -> [AT.Article] -> [AT.Article]
replaceArticle _ [] = []
replaceArticle a (h:t) | a ^. #slug == h ^. #slug = a:t
                       | otherwise = h : replaceArticle a t

updateFavorite r f (FavoriteArticle slug) gm =
  apiCall gm (r . ReceiveFavorite)
    $ A.favoriteArticle slug (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq)
updateFavorite r f (UnFavoriteArticle slug) gm =
  apiCall gm (r . ReceiveFavorite)
    $ A.unfavoriteArticle slug (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq)
updateFavorite r f (ReceiveFavorite e) gm = f e

-- TODO Find the right TZ
showTime t = toMisoString $ formatTime defaultTimeLocale "%B %e, %Y" t

viewArticle r a = do
  div_ [ class_ "article-preview" ]
       [ div_ [ class_ "article-meta" ]
         [ a_ [ href_ "" ] [ img_ [ src_ $ a ^. #author . #image ]]
         , div_ [ class_ "info" ]
           [ a_ [ href_  $ linkProfile $ AT.unUsername $ a ^. #author . #username
                , class_ "author" ]
                [ text $ AT.unUsername $ a ^. #author . #username  ]
           , span_ [ class_ "date" ]
                   [ text $ showTime $ a ^. #createdAt ]]
         , button_ 
           [ class_ $ "btn btn-sm pull-xs-right " <> if a ^. #favorited then
                                                       "btn-primary" else
                                                       "btn-outline-primary"
           , if a ^. #favorited then
               onClick $ localAction $ r $ UnFavoriteArticle $ a ^. #slug else
               onClick $ localAction $ r $ FavoriteArticle $ a ^. #slug]
           [ i_ [ class_ "ion-heart" ] []
           , text $ " " <> toMisoString (show (a ^. #favoritesCount)) ]
         ]
       , a_ [ href_ $ linkArticle $ a ^. #slug
            , class_ "preview-link" ]
         [ h1_ [] [ text $ a ^. #title ]
         , p_ [] [ text $ a ^. #description ]
         , span_ [] [ text "Read more..." ]
         , ul_ 
           [ class_ "tag-list" ]
           (map
           (\t -> li_ [ class_ "tag-default tag-pill tag-outline" ]
                     [ text t ])
             $ a ^. #tagList)]]

articlePagination r articlesPerPage model = 
  (if model ^. #totalArticleCount <= articlesPerPage then
                 [] else
                 [ ul_ [ class_ "pagination" ]
                   (map
                    (\p ->
                       li_ [ class_ $ "page-item" <> if pageNumber (model ^. #feedSelected) == p then " active" else ""]
                           [ a_ [ class_ "page-link"
                                -- , href_ ""  TODO
                                , onClick (localAction $ r $ setPage (model ^. #feedSelected) p)] [ text $ toMisoString $ show p ] ])
                    [1.. (ceiling (fromIntegral (model ^. #totalArticleCount) / fromIntegral articlesPerPage))])])

deleteComment :: Int -> [AT.Comment] -> [AT.Comment]
deleteComment n = Prelude.filter (\c -> c ^. #id /= n)

mapWithDefault :: b -> (a -> b) -> [a] -> [b]
mapWithDefault r _ [] = [r]
mapWithDefault _ f l  = map f l
