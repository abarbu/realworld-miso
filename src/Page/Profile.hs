module Page.Profile where
import Miso
import Miso.String hiding(map)
import Support
import qualified Api as A
import qualified Api.Types as AT
import Control.Lens

data Model = Model { failed :: Maybe MisoString
                   , profile :: Maybe AT.Profile
                   , articles :: [AT.Article]
                   , totalArticleCount :: Int
                   , feedSelected :: FeedSelected
                   }
  deriving (Show, Eq, Generic)

initialModel = Model { failed = Nothing, profile = Nothing, articles = [], totalArticleCount = 0, feedSelected = MyFeed 1 }

data Action = Create MisoString
            | ReceiveUserProfile (Either MisoString (A.Profile AT.Profile))
            | ReceiveArticles (Either MisoString A.Articles)
            | ASelectFeed ActionSelectFeed
            | AFollow ActionFollow
            | AFavorite ActionFavorite
  deriving (Show)

articlesPerPage = 5

update :: Action -> GlobalModel -> Transition (Action' Action) Model ()
update (Create username) gm = do
  put initialModel
  apiCall gm ReceiveUserProfile
    $ A.getProfile (AT.Username username) (mkAuthenticatedRequest (gm ^? #modelLoggedInUser . _Just . #token) A.authMaybeReq)
  apiCall gm ReceiveArticles
    $ A.getFeed (mkAuthenticatedRequest (gm ^? #modelLoggedInUser . _Just . #token) A.authMaybeReq)
                Nothing (Just $ AT.Username username) Nothing (Just 5) (Just 0)
update (ReceiveUserProfile (Right (A.Profile p))) _ = #profile .= Just p
update (ReceiveUserProfile (Left e)) _ = #failed .= Just e
update (ReceiveArticles (Right as)) _ = do
  #articles .= as ^. #articles
  #totalArticleCount .= as ^. #articlesCount
update (ReceiveArticles (Left e)) _ = #failed .= Just e
update (ASelectFeed a@(SelectFeed f)) gm = do
  #feedSelected .= f
  updateFeedSelected articlesPerPage ReceiveArticles a gm
update (AFollow f) gm = updateFollow ReceiveUserProfile f gm
update (AFavorite f) gm =
  updateFavorite AFavorite (\x -> case x of
                               Right (A.Article a) ->
                                 #articles %= replaceArticle a
                               Left e ->
                                 #failed .= Just e)
         f gm

view :: MisoString -> GlobalModel -> Model -> View (Either GlobalAction Action)
view username gmodel model = divKeyed_  (Key "profile-page")
  [ class_ "profile-page"
  , onCreated (localAction (Create username))
  ]
  (case (model ^. #profile, model ^. #failed) of
     (Nothing, Nothing) -> [div_ [] []]
     (_, Just f) -> [div_ [] [ text "Error loading profile"
                            , text f ]]
     (Just p, _) ->
       [ case failed model of
            Nothing -> div_ [] []
            Just t -> ul_ [ class_ "error-messages" ]
                         [ li_ [] [ text t ] ]
       , div_ [ class_ "user-info" ]
         [ div_ [ class_ "container" ]
           [ div_ [ class_ "row" ]
             [ div_ 
               [ class_ "col-xs-12 col-md-10 offset-md-1" ]
               [ img_ [ src_ $ p ^. #image , class_ "user-img" ]
               , h4_ [] [ text (AT.unUsername $ p ^. #username) ]
               , p_ [] [ text $ fromMaybe "" $ p ^. #bio]
               , case (gmodel ^? #modelLoggedInUser . _Just . #username == Just (p ^. #username)
                      , gmodel ^. #modelLoggedInUser
                      , p ^. #following) of
                   (True, _, _) -> div_ [] []
                   (_, Just _, False) -> button_ [ class_ "btn btn-sm btn-outline-secondary action-btn"
                                             , onClick (localAction $ AFollow $ Follow username) ]
                                     [ i_ [ class_ "ion-plus-round" ] []
                                     , text $ " Follow " <> AT.unUsername (p ^. #username)]
                   (_, Just _, True) -> button_ [ class_ "btn btn-sm btn-outline-secondary action-btn"
                                            , onClick (localAction $ AFollow $ Unfollow username) ]
                                    [ i_ [ class_ "ion-minus-round" ] []
                                    , text $ " Unfollow " <> AT.unUsername (p ^. #username)]
                   (_, Nothing, _) -> div_ [] []]]]]
       , div_ 
         [ class_ "container" ]
         [ div_ [ class_ "row" ]
           [ div_ [ class_ "col-xs-12 col-md-10 offset-md-1" ]
             ([ div_ [ class_ "articles-toggle" ]
               [ ul_ [ class_ "nav nav-pills outline-active" ]
                 [ li_ [ class_ "nav-item" ]
                   [ a_ 
                     [ class_ $ "nav-link" <> (case model ^. #feedSelected of
                                                 MyFeed _ -> " active"
                                                 _ -> "")
                     -- TODO This puts up the wrong cursor but something is broken when clicking
                     -- , href_ ""
                     , onClick (localAction $ ASelectFeed $ SelectFeed $ MyFeed 1)]
                     [ text "My Articles" ]]
                 , li_ [ class_ "nav-item" ]
                   [ a_ 
                     [ class_ $ "nav-link" <> (case model ^. #feedSelected of
                                                 FavoriteFeed _ _ -> " active"
                                                 _ -> "")
                     -- TODO This puts up the wrong cursor but something is broken when clicking
                     -- , href_ "" 
                     , onClick (localAction $ ASelectFeed $ SelectFeed $ FavoriteFeed (AT.Username username) 1)]
                     [ text "Favorited Articles" ]]]]
              ]
              <> mapWithDefault (text "No articles are here... yet.") (viewArticle AFavorite) (model ^. #articles)
              <> articlePagination (ASelectFeed . SelectFeed) 5 model)]]])
