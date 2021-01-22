module Page.CreateArticle where
import Miso
import Miso.String hiding(map)
import Support
import qualified Api as A
import qualified Api.Types as AT
import Control.Lens
import qualified Data.Map as M

data Model = Model { failed :: Maybe MisoString 
                   , fields :: M.Map Int MisoString }
  deriving (Show, Eq, Generic)

initialModel = Model { failed = Nothing
                     , fields = mempty }

data Action = Create (Maybe MisoString)
            | Destroy
            | ReceiveArticle (Either MisoString (A.Article AT.Article))
            | SubmitArticle
            | ReceiveArticleSubmitted (Either MisoString (A.Article AT.Article))
            | UpdateField Int MisoString
  deriving (Show)

update :: Action -> GlobalModel -> Transition (Action' Action) Model ()
update (Create Nothing) gm = put initialModel
update (Create (Just slug)) gm = do
  apiCall gm ReceiveArticle
    $ A.getArticle (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq) slug
update (ReceiveArticle (Right (A.Article a))) _ = do
  #fields . Control.Lens.at 0 ?= a ^. #title
  #fields . Control.Lens.at 1 ?= a ^. #description
  #fields . Control.Lens.at 2 ?= a ^. #body
  #fields . Control.Lens.at 3 ?= Miso.String.unwords (a ^. #tagList)
update (ReceiveArticle (Left e)) _ = #failed .= Just e
update Destroy gm = put initialModel
update SubmitArticle gm = do
  fs <- use #fields
  case (fs ^? ix 0, fs ^? ix 1, fs ^? ix 2) of
    (Just t, Just d, Just b) ->
      apiCall gm ReceiveArticleSubmitted
      $ A.postArticle (mkAuthenticatedRequest (gm ^?! #modelLoggedInUser . _Just . #token) A.authReq)
      $ A.Article $ AT.NewArticle t d b $ maybe [] Miso.String.words (fs ^? ix 3)
    (Nothing, _, _) -> runLocalAction $ ReceiveArticleSubmitted $ Left "Title can't be blank"
    (_, Nothing, _) -> runLocalAction $ ReceiveArticleSubmitted $ Left "Description can't be blank"
    (_, _, Nothing) -> runLocalAction $ ReceiveArticleSubmitted $ Left "Body can't be blank"
update (ReceiveArticleSubmitted (Right (A.Article a))) _ =
  runGlobalAction $ goArticle $ a ^. #slug
update (ReceiveArticleSubmitted (Left e)) _ = #failed .= Just e
update (UpdateField i s) _ = #fields . Control.Lens.at i ?= s
                                  
view :: Maybe MisoString -> GlobalModel -> Model -> View (Either GlobalAction Action)
view slug gmodel model = divKeyed_  (Key "create-article-page")
  [ class_ "editor-page"
  , onCreated (localAction $ Create slug)
  , onDestroyed (localAction Destroy)]
  [ case failed model of
            Nothing -> div_ [] []
            Just t -> ul_ [ class_ "error-messages" ]
                         [ li_ [] [ text t ] ]
  , div_ [ class_ "container page" ]
    [ div_ [ class_ "row" ]
      [ div_ [ class_ "col-md-10 offset-md-1 col-xs-12" ]
        [ form_ 
          [ action_ "javascript:void(0)" ]
          [ fieldset_ []
            [ fieldset_ 
              [ class_ "form-group" ]
              [ input_ 
                [ type_ "text"
                , class_ "form-control form-control-lg"
                , placeholder_ "Article Title"
                , value_ (fromMaybe "" (model ^? #fields . ix 0))
                , onInput $ localAction . UpdateField 0 ]
              ]
            , fieldset_ 
              [ class_ "form-group" ]
              [ input_ 
                [ type_ "text"
                , class_ "form-control"
                , placeholder_ "What's this article about?"
                , value_ (fromMaybe "" (model ^? #fields . ix 1))
                , onInput $ localAction . UpdateField 1 ]
              ]
            , fieldset_ 
              [ class_ "form-group" ]
              [ textarea_ 
                [ class_ "form-control"
                , rows_ "8"
                , placeholder_ "Write your article (in markdown)"
                , value_ (fromMaybe "" (model ^? #fields . ix 2))
                , onInput $ localAction . UpdateField 2 ]
                []
              ]
            , fieldset_ 
              [ class_ "form-group" ]
              [ input_ 
                [ type_ "text"
                , class_ "form-control"
                , placeholder_ "Enter tags"
                , value_ (fromMaybe "" (model ^? #fields . ix 3))
                , onInput $ localAction . UpdateField 3 ]
              , div_ [ class_ "tag-list" ] []]
            , button_ 
              [ class_ "btn btn-lg pull-xs-right btn-primary"
              , type_ "button"
              , onClick (localAction SubmitArticle)]
              [ text "Publish Article" ]]]]]]]
