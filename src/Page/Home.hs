module Page.Home where
import           Miso
import           Miso.String hiding(map)
import Support
import qualified Api as A
import qualified Api.Types as AT
import Control.Lens

data Model = Model { failed :: Maybe MisoString
                   , modelTags :: Loadable MisoString [MisoString]
                   , articles :: Loadable MisoString [AT.Article]
                   , totalArticleCount :: Int
                   , feedSelected :: FeedSelected
                   }
  deriving (Show, Eq, Generic)

initialModel = Model { failed = Nothing
                     , modelTags = Loading
                     , articles = Loading
                     , totalArticleCount = 0
                     , feedSelected = GlobalFeed 1
                     }

data Action = Create
            | ReceiveTags (Either MisoString AT.Tags)
            | ReceiveArticles (Either MisoString A.Articles)
            | ASelectFeed ActionSelectFeed
            | AFavorite ActionFavorite
  deriving (Show)

articlesPerPage = 10

update :: Action -> GlobalModel -> Transition (Action' Action) Model ()
update Create gm = do
  put initialModel
  apiCall gm ReceiveTags A.getTags
  -- TODO Get the feed for the user later
  apiCall gm ReceiveArticles
    $ A.getFeed (mkAuthenticatedRequest (gm ^? #modelLoggedInUser . _Just . #token) A.authMaybeReq)
                Nothing Nothing Nothing (Just articlesPerPage) (Just 0)
update (ReceiveTags (Right tags)) gm =
  #modelTags .= Loaded (getField @"tags" tags)
update (ReceiveTags (Left e)) gm =
  #modelTags .= FailedLoading e
update (ReceiveArticles (Right as)) _ = do
  #articles .= Loaded (as ^. #articles)
  #totalArticleCount .= as ^. #articlesCount
update (ReceiveArticles (Left e)) _ = #failed .= Just e
update (ASelectFeed a@(SelectFeed f)) gm = do
  #feedSelected .= f
  #articles .= Loading
  updateFeedSelected articlesPerPage ReceiveArticles a gm
update (AFavorite f) gm =
  updateFavorite AFavorite (\case
                               Right (A.Article a) ->
                                 #articles . _Loaded %= replaceArticle a
                               Left e ->
                                 #failed .= Just e)
         f gm

view :: GlobalModel -> Model -> View (Either GlobalAction Action)
view gmodel model = divKeyed_  (Key "home-page")
  [ class_ "home-page",
    onCreated (localAction Create)
  ]
  [ div_ [ class_ "banner" ]
    [ div_ [ class_ "container" ]
      [ h1_ [ class_ "logo-font" ]
            [ text "conduit" ]
      , p_ [] [ text "A place to share your knowledge." ]]]
  , div_ 
    [ class_ "container page" ]
    [ div_ 
      [ class_ "row" ]
      [ div_ 
        [ class_ "col-md-9" ]
        [ div_ 
          [ class_ "feed-toggle" ]
          ([ ul_ [ class_ "nav nav-pills outline-active" ]
            ([ li_ [ class_ "nav-item" ]
              [ a_ ([ class_ $ "nav-link" <> (case model ^. #feedSelected of
                                                MyFeed _ -> " active"
                                                _ -> "") <> maybe " disabled" (const "") (gmodel ^. #modelLoggedInUser)]
                    <>
                   -- TODO This puts up the wrong cursor but something is broken when clicking
                   -- , href_ ""
                    (case gmodel ^? #modelLoggedInUser . _Just . #username of
                       Just (AT.Username u) -> [onClick (localAction $ ASelectFeed $ SelectFeed $ MyFeed 1)]
                       _ -> [])
                   )
                [ text "Your Feed" ]]
            , li_ [ class_ "nav-item" ]
              [ a_ [ class_ $ "nav-link" <> (case model ^. #feedSelected of
                                               GlobalFeed _ -> " active"
                                               _ -> "")
                  -- TODO This puts up the wrong cursor but something is broken when clicking
                  -- , href_ "" 
                   , onClick (localAction $ ASelectFeed $ SelectFeed $ GlobalFeed 1)
                   ]
                [ text "Global Feed" ]]
            ]
            <>
            (case model ^. #feedSelected of
                TagFeed s _ ->
                  [ li_ 
                    [ class_ "nav-item" ]
                    [ a_ 
                      [ class_ "nav-link active"
                      -- , href_ ""  TODO
                      , onClick (localAction $ ASelectFeed $ SelectFeed $ TagFeed s 1) ]
                      [ i_ [ class_ "ion-pound" ] []
                      , text $ " " <> s]]]
                _ -> []))]
        <> loadable [text "Loading ..."] ((:[]).text)
                    (mapWithDefault (text "No articles are here... yet.") (viewArticle AFavorite))
                    (model ^. #articles)
        <> (if isLoaded (model ^. #articles) then articlePagination (ASelectFeed . SelectFeed) 10 model else []))]
      , div_ 
        [ class_ "col-md-3" ]
        [ div_ 
          [ class_ "sidebar" ]
          [ p_ [] [ text "Popular Tags" ]
          , div_ 
            [ class_ "tag-list" ]
            (loadable
             [text "Loading ..."]
             ((:[]) . text)
             (map (\s -> a_ 
                        [ class_ "tag-pill tag-default"
                        , onClick $ localAction $ ASelectFeed $ SelectFeed (TagFeed s 1) ]
                        [ text s ]))
             (modelTags model)) ]]]]]

