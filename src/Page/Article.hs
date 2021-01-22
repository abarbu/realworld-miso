module Page.Article where
import Miso
import Miso.String hiding(map)
import qualified Api as A
import qualified Api.Types as AT
import Support
import Cheapskate
import qualified Text.Blaze.Html.Renderer.Text as B
import qualified Text.Blaze.Html as B
import qualified Data.Aeson as A

data Model = Model { failed :: Maybe MisoString
                   , article :: Maybe AT.Article
                   , comments :: [AT.Comment]
                   , commentField :: MisoString
                   }
  deriving (Show, Eq, Generic)

initialModel = Model { failed = Nothing, article = Nothing, comments = [], commentField = "" }

data Action = Create MisoString
            | ReceiveArticle (Either MisoString (A.Article AT.Article))
            | ReceiveComments (Either MisoString (A.Comments AT.Comment))
            | ReceiveUserProfile (Either MisoString (A.Profile AT.Profile))
            | AFollow ActionFollow
            | AFavorite ActionFavorite
            | UpdateComment MisoString
            | SubmitComment MisoString
            | ReceiveComment (Either MisoString (A.Comment AT.Comment))
            | DeleteComment MisoString Int
            | ReceiveDeletedComment Int (Either MisoString A.Value)
            | DeleteArticle MisoString
            | ReceiveDeleteArticle (Either MisoString A.Value)
  deriving (Show)

update :: Action -> GlobalModel -> Transition (Action' Action) Model ()
update (Create slug) gm = do
  put initialModel
  #commentField .= ""
  apiCall gm ReceiveArticle
    $ A.getArticle (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq) slug
  apiCall gm ReceiveComments
    $ A.getComments slug (mkAuthenticatedRequest (gm ^? #modelLoggedInUser . _Just . #token) A.authMaybeReq)
update (ReceiveArticle (Right (A.Article a))) _ = #article .= Just a
update (ReceiveArticle (Left e)) _ = #failed .= Just e
update (ReceiveComments (Right (A.Comments cs))) gm = #comments .= cs
update (ReceiveComments (Left e)) _ = #failed .= Just e
update (AFollow f) gm = updateFollow ReceiveUserProfile f gm
update (AFavorite f) gm =
  updateFavorite AFavorite (\case
                               Right (A.Article a) -> #article .= Just a
                               Left e -> #failed .= Just e)
         f gm
update (ReceiveUserProfile (Right (A.Profile p))) _ = #article . _Just . #author .= p
update (ReceiveUserProfile (Left e)) _ = #failed .= Just e
update (UpdateComment s) _ = #commentField .= s
update (SubmitComment slug) gm = do
  b <- use #commentField
  apiCall gm ReceiveComment
    $ A.postComment slug (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq)
    $ A.Comment
    $ AT.NewComment b
update (ReceiveComment (Right (A.Comment c))) _ = #comments <>= [c]
update (ReceiveComment (Left e)) _ = #failed .= Just e
update (DeleteComment slug i) gm = do
  apiCall gm (ReceiveDeletedComment i)
    $ A.deleteComment slug (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq) i
update (ReceiveDeletedComment n (Right _)) _ = do
  #comments %= deleteComment n
update (ReceiveDeletedComment _ (Left e)) _ = #failed .= Just e
update (DeleteArticle slug) gm = do
  apiCall gm ReceiveDeleteArticle
    $ A.deleteArticle (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq) slug
update (ReceiveDeleteArticle (Right _)) _ = scheduleIO $ pure $ globalAction goHome
update (ReceiveDeleteArticle (Left e)) _ = #failed .= Just e

innerHTML_ ::  MisoString -> Attribute action
innerHTML_ = textProp "innerHTML"

view :: MisoString -> GlobalModel -> Model -> View (Either GlobalAction Action)
view slug gmodel model = divKeyed_  (Key "article-page")
  [ class_ "article-page"
  , onCreated (localAction (Create slug))
  ]
  ([case failed model of
            Nothing -> div_ [] []
            Just t -> ul_ [ class_ "error-messages" ]
                         [ li_ [] [ text t ] ]]
   <>
   (case model ^. #article of
     Nothing -> []
     Just a ->
       let userButtons =
             ([ a_ [ href_ $ linkProfile $ AT.unUsername $ a ^. #author . #username ]
                   [ img_ [ src_ $ a ^. #author . #image ]]
             , div_ [ class_ "info" ]
               [ a_ [ href_ $ linkProfile $ AT.unUsername $ a ^. #author . #username
                    , class_ "author" ]
                    [ text $ AT.unUsername $ a ^. #author . #username  ]
               , span_ [ class_ "date" ] [ text $ showTime $ a ^. #updatedAt ]]]
             <>
             (case gmodel ^. #modelLoggedInUser of
               Nothing -> []
               Just u ->
                 if u ^. #username == a ^. #author . #username then
                   [ button_ 
                     [ class_ "btn btn-sm btn-outline-secondary"
                       , onClick (globalAction $ goArticleEditor $ a ^. #slug)]
                     [ i_ [ class_ "ion-edit" ] []
                     , text "Edit Article"]
                   , text "  "
                   , button_ 
                     [ class_ $ "btn btn-sm btn-outline-danger"
                     , onClick $ localAction $ DeleteArticle $ a ^. #slug]
                     [ i_ [ class_ "ion-trash-a" ] []
                     , text " Delete Article" ]]
                 else
                   [ button_ 
                     [ class_ "btn btn-sm btn-outline-secondary"
                       , onClick (localAction $ AFollow $
                                  (if a ^. #author . #following then
                                     Unfollow else
                                     Follow)
                                  $ AT.unUsername (a ^. #author . #username))]
                     [ i_ [ class_ $ if a ^. #author . #following then
                              "ion-minus-round" else
                              "ion-plus-round"
                          ] []
                     , text $ (if a ^. #author . #following then
                                 " Unfollow " else
                                 " Follow ") <> AT.unUsername (a ^. #author . #username) ]
                   , text "  "
                   , button_ 
                     [ class_ $ "btn btn-sm " <> if a ^. #favorited then
                                                   "btn-primary" else
                                                   "btn-outline-primary"
                     , if a ^. #favorited then
                         onClick $ localAction $ AFavorite $ UnFavoriteArticle $ a ^. #slug else
                         onClick $ localAction $ AFavorite $ FavoriteArticle $ a ^. #slug ]
                     [ i_ [ class_ "ion-heart" ] []
                     , text $ " Favorite Article (" <> toMisoString (show (a ^. #favoritesCount)) <> ")" ]
                   ]))
       in 
       [ div_ [ class_ "banner" ]
         [ div_ [ class_ "container" ]
           [ h1_ [] [ text $ a ^. #title ]
           , div_ [ class_ "article-meta" ]
             userButtons]]
       , div_ 
         [ class_ "container page" ]
         [ div_ [ class_ "row article-content" ]
           [ div_ [ class_ "col-md-12" ]
             [ div_ [ innerHTML_ (toMisoString $ B.renderHtml $ B.toHtml $ markdown def (fromMisoString $ a ^. #body)) ] [] ]
           ]
         , hr_ []
         , div_ [ class_ "article-actions" ]
                [ div_ [ class_ "article-meta" ] userButtons ]
         , div_ [ class_ "row" ]
           [ div_ [ class_ "col-xs-12 col-md-8 offset-md-2" ]
             ([ form_ 
               [ action_ "javascript:void(0)" -- TODO Why do we need this?
               , class_ "card comment-form" ]
               [ div_ [ class_ "card-block" ]
                 [ textarea_ 
                   [ class_ "form-control"
                   , placeholder_ "Write a comment..."
                   , rows_ "3"
                   , onInput $ localAction . UpdateComment ]
                   []]
               , div_ [ class_ "card-footer" ]
                 [ img_ [ src_ $ a ^. #author . #image
                        , class_ "comment-author-img" ]
                 , button_ 
                   [ class_ "btn btn-sm btn-primary"
                   , onClick $ localAction $ SubmitComment slug]
                   [ text "Post Comment" ]]]]
             <>
             map (\c ->
                    div_ [ class_ "card" ]
                   [ div_ [ class_ "card-block" ]
                          [ p_ [ class_ "card-text" ] [ text $ c ^. #body ]]
                   , div_ [ class_ "card-footer" ]
                     [ a_ [ href_ "" , class_ "comment-author" ]
                          [ img_ [ src_ $ c ^. #author . #image
                                 , class_ "comment-author-img" ]]
                     , text " "
                     , a_ [ href_ ""
                          , class_ "comment-author" ]
                          [ text $ AT.unUsername (c ^. #author . #username) ]
                     , span_ [ class_ "date-posted" ]
                             [ text $ showTime $ c ^. #updatedAt ]
                     , if gmodel ^? #modelLoggedInUser . _Just . #username == Just (c ^. #author . #username) then
                         span_ [ class_ "mod-options"
                               , onClick $ localAction $ DeleteComment slug (c ^. #id)]
                               [ i_ [ class_ "ion-trash-a" ] [] ]
                         else div_ [] []]])
               (model ^. #comments))]]]))
