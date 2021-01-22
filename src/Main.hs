module Main where
import Miso hiding (runRoute)
import Miso.String
import Servant.API hiding(layout)
#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import Network.WebSockets
#endif
import Types
import Support
import qualified Page.Home
import qualified Page.Auth
import qualified Page.CreateArticle
import qualified Page.Article
import qualified Page.Profile
import qualified Page.Settings
import Control.Lens as L
import qualified Servant.Client.JSaddle as S
import Data.Proxy
import Utils.Miso.Router
import qualified Api.Types as AT

apiBaseUrl = S.parseBaseUrl "https://conduit.productionready.io/"

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f =
  Warp.runSettings (Warp.setPort 8080 (Warp.setTimeout 3600 Warp.defaultSettings)) =<<
    JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

viewRouter handlers model =
  either (const the404) id
  $ runRoute (Proxy @(API Action))
             (const (divKeyed_ (Key "init") [onCreated (Global goHome)] []) :<|> handlers)
             (modelURI . modelGlobal) model
  where the404 = div_ [] [text "the 404 :("
                         , button_ [ onClick (Global goHome) ] [ text "go home" ] ]

main :: IO ()
main = runApp $ do
    currentURI <- getCurrentURI
    baseUrl <- apiBaseUrl
    startApp App { initialAction = Global Start
                 , model  = Model { modelGlobal =
                                    GlobalModel
                                    { modelURI          = currentURI
                                    , modelLoggedInUser = Nothing
                                    , modelBaseUrl      = baseUrl
                                    , modelStatus       = StartingGlobal
                                    }
                                  , modelRouteHome = Page.Home.initialModel
                                  , modelRouteLogin = Page.Auth.initialModel
                                  , modelRouteRegister = Page.Auth.initialModel
                                  , modelRouteCreateArticle = Page.CreateArticle.initialModel
                                  , modelRouteArticle = Page.Article.initialModel
                                  , modelRouteProfile = Page.Profile.initialModel
                                  , modelRouteSettings = Page.Settings.initialModel
                                  }
                 , update = fromTransition . updateModel
                 , view   = layout (viewRouter
                                    (pageView modelRouteHome HomeRoute Page.Home.view :<|>
                                     pageView modelRouteLogin LoginRoute Page.Auth.view :<|>
                                     pageView modelRouteRegister RegisterRoute Page.Auth.view :<|>
                                     pageView modelRouteSettings SettingsRoute Page.Settings.view :<|>
                                     (pageView modelRouteArticle ArticleRoute . Page.Article.view) :<|>
                                     pageView modelRouteCreateArticle CreateArticleRoute (Page.CreateArticle.view Nothing) :<|>
                                     (pageView modelRouteCreateArticle CreateArticleRoute . Page.CreateArticle.view . Just) :<|>
                                     (pageView modelRouteProfile ProfileRoute . Page.Profile.view) :<|>
                                     (\j m -> undefined)))
                 , events = defaultEvents
                 , subs   = [ uriSub (Global . HandleURI) ]
                 , mountPoint = Nothing
                 , logLevel = Off
                 }

updateModel :: Action -> Transition Action Model ()
updateModel (Global Start) = do
  scheduleIO $ Global . LoadedUserFromStorage <$> getLocalStorage "realworld.miso.user"
updateModel (Global (LoadedUserFromStorage (Right u))) = do
  #modelGlobal . #modelStatus .= ReadyGlobal
  scheduleIO $ pure $ Global $ UserLoggedIn u
updateModel (Global (LoadedUserFromStorage _)) =
  #modelGlobal . #modelStatus .= ReadyGlobal
updateModel (Global (ChangeURI u)) = do
  -- TODO Why is this assignment required here? Why doesn't HandleURI work?
  #modelGlobal . #modelURI .= u
  scheduleIO_ $ replaceURI u
updateModel (Global (HandleURI u))    = do
  scheduleIO_ $ consoleLog $ toMisoString $ show u
  #modelGlobal . #modelURI .= u
updateModel (Global (UserLoggedIn u)) = do
  #modelGlobal . #modelLoggedInUser .= Just u
  scheduleIO_ $ setLocalStorage "realworld.miso.user" u
updateModel (Global UserLoggedOut)    = do
  #modelGlobal . #modelLoggedInUser .= Nothing
  scheduleIO_ $ removeLocalStorage "realworld.miso.user"
updateModel (HomeRoute a)          = pageUpdate HomeRoute          #modelRouteHome           a Page.Home.update
updateModel (LoginRoute a)         = pageUpdate LoginRoute         #modelRouteLogin          a Page.Auth.update
updateModel (RegisterRoute a)      = pageUpdate RegisterRoute      #modelRouteRegister       a Page.Auth.update
updateModel (CreateArticleRoute a) = pageUpdate CreateArticleRoute #modelRouteCreateArticle  a Page.CreateArticle.update
updateModel (ArticleRoute a)       = pageUpdate ArticleRoute       #modelRouteArticle        a Page.Article.update
updateModel (ProfileRoute a)       = pageUpdate ProfileRoute       #modelRouteProfile        a Page.Profile.update
updateModel (SettingsRoute a)      = pageUpdate SettingsRoute      #modelRouteSettings       a Page.Settings.update

layout :: (Model -> View Action) -> Model -> View Action
layout contents model = div_ []
  [ link_ [ rel_ "stylesheet"
          , href_ "//demo.productionready.io/main.css" ]
  , link_ [ rel_ "stylesheet"
          , href_ "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic" ]
  , link_ [ rel_ "stylesheet"
          , href_ "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css" ]
  , nav_ 
      [ class_ "navbar navbar-light" ]
      [ div_ 
        [ class_ "container" ]
        [ a_ [ class_ "navbar-brand" , href_ "/" ]
             [ text "conduit" ]
        , ul_ 
          [ class_ "nav navbar-nav pull-xs-right" ]
          (let item label action goto icon =
                 li_ [ class_ "nav-item" ]
                     [ a_ [ classList_ [("nav-link", True),
                                        ("active", isOnPage model goto)],
                            href_ "#",
                            action goto ]
                       (case icon of
                          Nothing -> [ text label ]
                          Just (Left icon) -> [ i_ [ class_ icon ] []
                                             , text ("\160" <> label) ]
                          Just (Right image) -> [ img_ [ class_ "user-pic", src_ image ]
                                               , text label ])]
           in case modelLoggedInUser $ modelGlobal model of
                Nothing -> [item "Home"    href_ linkHome     Nothing
                          ,item "Sign in" href_ linkLogin    Nothing
                          ,item "Sign up" href_ linkRegister Nothing]
                Just u -> [item "Home"      href_ linkHome       Nothing
                         ,item "New Post"  href_ linkEditor    (Just (Left "ion-compose"))
                         ,item "Settings"  href_ linkSettings  (Just (Left "ion-gear-a"))
                         ,item (AT.unUsername $ getField @"username" u)
                               href_
                               (linkProfile (AT.unUsername $ getField @"username" u))
                               (Right <$> (getField @"image" u <|> Just "https://static.productionready.io/images/smiley-cyrus.jpg"))
                         ,item "Sign out"
                               (const $ onClick (Global UserLoggedOut))
                               ""
                               Nothing])]]
    , case modelStatus $ modelGlobal model of
        ReadyGlobal -> contents model
        StartingGlobal -> div_ [] []
    , footer_ []
      [ div_ 
        [ class_ "container" ]
        [ a_ [ href_ "/" , class_ "logo-font" ]
             [ text "conduit" ]
        , span_ 
          [ class_ "attribution" ]
          [ text "An interactive learning project from "
          , a_ [ href_ "https://thinkster.io" ]
               [ text "Thinkster" ]
          , text ". Code & design licensed under MIT."]]]]
