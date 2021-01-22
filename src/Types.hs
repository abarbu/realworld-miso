module Types where
import GHC.Generics (Generic)
import Miso
import           Miso.String
import qualified Page.Home
import qualified Page.Auth
import qualified Page.CreateArticle
import qualified Page.Article
import qualified Page.Profile
import qualified Page.Settings
import Control.Lens as L
import Misc

data Model = Model { modelGlobal :: GlobalModel,
                     modelRouteHome :: Page.Home.Model,
                     modelRouteLogin :: Page.Auth.Model,
                     modelRouteRegister :: Page.Auth.Model,
                     modelRouteCreateArticle :: Page.CreateArticle.Model,
                     modelRouteArticle :: Page.Article.Model,
                     modelRouteProfile :: Page.Profile.Model,
                     modelRouteSettings :: Page.Settings.Model
                   }
  deriving (Eq, Generic)

data Action
  = Global GlobalAction
  | HomeRoute Page.Home.Action
  | LoginRoute Page.Auth.Action
  | RegisterRoute Page.Auth.Action
  | CreateArticleRoute Page.CreateArticle.Action
  | ArticleRoute Page.Article.Action
  | ProfileRoute Page.Profile.Action
  | SettingsRoute Page.Settings.Action

liftTransition :: (Action' pageAction -> Action) -> Lens' Model pageModel -> Transition (Action' pageAction) pageModel () -> Transition Action Model ()
liftTransition toA modelLens t =
  toTransition (\m -> bimap toA
                           (flip (L.set modelLens) m)
                           (fromTransition t $ L.view modelLens m))

pageUpdate :: (pageAction -> Action) -> Lens' Model pageModel -> pageAction
           -> (pageAction -> GlobalModel -> Transition (Action' pageAction) pageModel ())
           -> Transition Action Model ()
pageUpdate toA modelLens a tf = do
  mg <- use #modelGlobal
  liftTransition (\case
                     Left g -> Global g
                     Right a -> toA a) modelLens $ tf a mg

pageView :: (Model -> model) -> (action -> Action) -> (GlobalModel -> model -> View (Either GlobalAction action)) -> Model -> View Action
pageView fm fa view m = (\case
                            Left a -> Global a
                            Right a -> fa a) <$> view (modelGlobal m) (fm m)

isOnPage model fragment = modelUriFragment model == (fragment <> "/") || modelUriFragment model == fragment

modelUriFragment model = toMisoString $ uriFragment $ modelURI $ modelGlobal model
                   
