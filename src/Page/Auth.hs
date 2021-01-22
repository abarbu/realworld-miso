module Page.Auth where
import Miso
import Miso.String hiding(map)
import Support
import qualified Api as A
import qualified Api.Types as AT
import qualified Data.Map as M
import Control.Lens

data Model = Model { failedLogin :: Maybe MisoString 
                   , fields :: M.Map Int MisoString }
  deriving (Show, Eq, Generic)

initialModel = Model { failedLogin = Nothing
                     , fields = mempty }

data Action = Create
            | Destroy
            | RequestLogin
            | ReceiveLogin (Either MisoString (A.User AT.User))
            | RequestRegistration
            | ReceiveRegistration (Either MisoString (A.User AT.User))
            | UpdateField Int MisoString
  deriving (Show)

update :: Action -> GlobalModel -> Transition (Action' Action) Model ()
update Create _  = put initialModel
update Destroy _ = put initialModel
update RequestLogin gm = do
  #failedLogin .= Nothing
  fs <- use #fields
  case (fs ^? ix 1, fs ^? ix 2) of
    (Just e, Just p) ->
      apiCall gm ReceiveLogin
        $ A.loginUser
        $ A.User $ AT.LoginUser (AT.Email e) (AT.Password p)
    (Nothing, _) -> runLocalAction $ ReceiveLogin $ Left "Email can't be blank"
    (_, Nothing) -> runLocalAction $ ReceiveLogin $ Left "Password can't be blank"
update (ReceiveLogin (Right (A.User u))) _ = do
  runGlobalAction $ UserLoggedIn u
  runGlobalAction goHome
update (ReceiveLogin (Left e)) _ = do
  #failedLogin .= Just e
update RequestRegistration gm = do
  #failedLogin .= Nothing
  fs <- use #fields
  case (fs ^? ix 0, fs ^? ix 1, fs ^? ix 2) of
    (Just u, Just e, Just p) ->
      apiCall gm ReceiveRegistration
        $ A.registerUser
        $ A.User $ AT.NewUser (AT.Username u) (AT.Email e) (AT.Password p)
    (Nothing, _, _) -> runLocalAction $ ReceiveRegistration $ Left "Username can't be blank"
    (_, Nothing, _) -> runLocalAction $ ReceiveRegistration $ Left "Email can't be blank"
    (_, _, Nothing) -> runLocalAction $ ReceiveRegistration $ Left "Password can't be blank"
update (ReceiveRegistration (Right (A.User u))) _ = do
  runGlobalAction $ UserLoggedIn u
  runGlobalAction goHome
update (ReceiveRegistration (Left e)) _ = #failedLogin .= Just e
update (UpdateField i s) _ = #fields . Control.Lens.at i ?= s

view :: GlobalModel -> Model -> View (Either GlobalAction Action)
view gmodel model = divKeyed_ (Key $ globalFragment gmodel)
  [ class_ "auth-page"
  , onCreated (localAction Create)
  , onDestroyed (localAction Destroy) ]
  [ div_ [ class_ "container page" ]
    [ div_ [ class_ "row" ]
      [ div_ [ class_ "col-md-6 offset-md-3 col-xs-12" ]
        [ h1_ [ class_ "text-xs-center"]
          [ text (if linkLogin == here then "Sign in" else "Sign up") ]
        , p_ [ class_ "text-xs-center" ]
          [ a_ [ href_ (if linkLogin == here then
                          linkRegister else
                          linkLogin) ]
            [ text (if linkLogin == here then
                       "Need an account?" else
                       "Have an account?")] ]
        , case failedLogin model of
            Nothing -> div_ [] []
            Just t -> ul_ [ class_ "error-messages" ]
                         [ li_ [] [ text t ] ]
        , form_
          -- TODO Why is this needed? Shuouldn't onSubmit supress the event?
          -- But onSubmit doesn't seem to work and onClick doesn't
          -- stop events even when we write a custom one that does.
          -- This is used in many places, look for all of them.
          [ action_ "javascript:void(0)" ]
          [ if linkLogin == here then
              div_ [] [] else
              fieldset_ [ class_ "form-group" ]
              [ input_ 
                [ class_ "form-control form-control-lg"
                , type_ "text"
                , placeholder_ "Username"
                , onInput $ localAction . UpdateField 0 ]]
          , fieldset_ [ class_ "form-group" ]
            [ input_ [ class_ "form-control form-control-lg"
                     , type_ "text"
                     , placeholder_ "Email" 
                     , onInput $ localAction . UpdateField 1 ] ]
          , fieldset_ [ class_ "form-group" ]
            [ input_ [ class_ "form-control form-control-lg"
                     , type_ "password"
                     , placeholder_ "Password"
                     , onInput $ localAction . UpdateField 2 ] ]
          , button_ [ class_ "btn btn-lg btn-primary pull-xs-right"
                    , onClick (localAction (if linkLogin == here then
                                              RequestLogin else
                                              RequestRegistration)) ]
            [ text (if linkLogin == here then "Sign in" else "Sign up") ]]]]]]
  where here = globalFragment gmodel
