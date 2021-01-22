module Page.Settings where
import Miso
import Miso.String hiding(map)
import Support
import qualified Api as A
import qualified Data.Map as M
import qualified Api.Types as AT
import Control.Lens

data Model = Model { failed :: Maybe MisoString
                   , fields :: M.Map Int MisoString
                    }
  deriving (Show, Eq, Generic)

initialModel = Model { failed = Nothing
                     , fields = mempty }

data Action = Create
            | Destroy
            | UpdateField Int MisoString
            | ReceiveUserUpdate Bool (Either MisoString (A.User AT.User))
            | Submit
  deriving (Show)

update :: Action -> GlobalModel -> Transition (Action' Action) Model ()
update Create gm = do
  put initialModel
  let (Just u) = gm ^. #modelLoggedInUser
  apiCall gm (ReceiveUserUpdate False)
    $ A.getUser (mkAuthenticatedRequest (u ^. #token) A.authReq)
update Destroy _ = put initialModel
update (UpdateField i s) _ = #fields . Control.Lens.at i ?= s
update Submit gm = do
  #failed .= mempty
  fs <- use #fields
  let (Just u) = gm ^. #modelLoggedInUser
  apiCall gm (ReceiveUserUpdate True)
    $ A.updateUser (mkAuthenticatedRequest (u ^. #token) A.authReq) (A.User (AT.UpdateUser
                             (maybe (u ^. #email) AT.Email (fs ^? ix 3))
                             (maybe (u ^. #username) AT.Username (fs ^? ix 1))
                             (fs ^? ix 2)
                             (fs ^? ix 0)
                             (fs ^? ix 4)))
update (ReceiveUserUpdate b (Right (A.User u))) _ = do
  scheduleIO $ pure $ globalAction $ UserLoggedIn u
  #fields .= mempty
  when b $ scheduleIO $ pure $ globalAction goHome
update (ReceiveUserUpdate _ (Left e)) _ = #failed .= Just e

view :: GlobalModel -> Model -> View (Either GlobalAction Action)
view gmodel model = divKeyed_ (Key "settings-page")
  [ class_ "settings-page"
  , onCreated (localAction Create)
  , onDestroyed (localAction Destroy) ]
  [ case failed model of
            Nothing -> div_ [] []
            Just t -> ul_ [ class_ "error-messages" ]
                         [ li_ [] [ text t ] ]
  , div_ [ class_ "container page" ]
    [ div_ [ class_ "row" ]
      [ div_ [ class_ "col-md-6 offset-md-3 col-xs-12" ]
        [ h1_ [ class_ "text-xs-center" ]
          [ text "Your Settings" ]
        , form_ [ action_ "javascript:void(0)" ]
          [ fieldset_ []
            [ fieldset_ [ class_ "form-group" ]
              [ controlledInput_ model [class_ "form-control", type_ "text"]
                "URL of profile picture"
                0
                (getField @"image" =<< modelLoggedInUser gmodel)
                UpdateField ]
            , fieldset_ [ class_ "form-group" ]
            -- TODO Switch all of this to controlledInput_
              [ controlledInput_ model [ class_ "form-control form-control-lg", type_ "text"]
                "Your Name"
                1
                (AT.unUsername . getField @"username" <$> modelLoggedInUser gmodel)
                UpdateField
              ]
            , fieldset_ [ class_ "form-group" ]
              [ textarea_ 
                [ class_ "form-control form-control-lg"
                , rows_ "8"
                , placeholder_ "Short bio about you" 
                , value_ (fromMaybe "" ((model ^? (#fields . ix 2))
                                        <|>
                                        (getField @"bio" =<< modelLoggedInUser gmodel)))
                , onInput $ localAction . UpdateField 2 ]
                []
              ]
            , fieldset_ [ class_ "form-group" ]
              [ controlledInput_ model [ class_ "form-control form-control-lg", type_ "text"]
                "Email"
                3
                (AT.unEmail . getField @"email" <$> modelLoggedInUser gmodel)
                UpdateField
              ]
            , fieldset_ [ class_ "form-group" ]
              [ controlledInput_ model [ class_ "form-control form-control-lg", type_ "password"]
                "Password"
                4
                Nothing
                UpdateField
              ]
            , button_ [ class_ "btn btn-lg btn-primary pull-xs-right"
                      , onClick (localAction Submit) ]
              [ text "Update Settings" ]]]]]]]
