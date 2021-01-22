{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
module Misc where
import           Miso.String
import qualified Servant.API as S
import qualified Servant.Client.JSaddle as S
import GHC.Generics (Generic)
import qualified GHCJS.DOM.XMLHttpRequest as JS
import           Servant.Client.JSaddle
import           Miso
import qualified Api.Types as AT
import Data.Maybe
import Control.Lens
import Control.Applicative
import qualified Servant.API.Experimental.Auth

data Loadable err val = Loading
                      | Loaded val
                      | FailedLoading err
  deriving (Eq, Show)
makePrisms ''Loadable

loadable :: a -> (err -> a) -> (val -> a) -> Loadable err val -> a
loadable loading _    f (Loaded l)        = f l
loadable loading fmsg _ (FailedLoading a) = fmsg a
loadable loading _ _    Loading           = loading

isLoaded (Loaded _) = True
isLoaded _          = False

data GlobalStatus = StartingGlobal | ReadyGlobal
  deriving (Eq, Generic)

data GlobalModel = GlobalModel
  { modelURI :: S.URI,
    modelLoggedInUser :: Maybe AT.User,
    modelBaseUrl :: S.BaseUrl,
    modelStatus :: GlobalStatus
  }
  deriving (Eq, Generic)

data GlobalAction
  = -- | For the router
    ChangeURI S.URI
  | HandleURI S.URI
    -- | User lifecycle
  | UserLoggedIn AT.User
  | UserLoggedOut
  | LoadedUserFromStorage (Either String AT.User)
    --
  | Start

type Action' a = Either GlobalAction a
type Model' a = Either GlobalModel a

localAction :: a -> Action' a
localAction = Right 
globalAction :: GlobalAction -> Action' a
globalAction = Left

localModel :: (x -> a) -> x -> Model' a
localModel f = Right . f
globalModel :: (x -> GlobalModel) -> x -> Model' a
globalModel f = Left . f

runLocalAction a = scheduleIO $ pure $ localAction a
runGlobalAction a = scheduleIO $ pure $ globalAction a

apiCall globalModel_ localAction_ =
  runClient (modelBaseUrl globalModel_) (localAction . localAction_ . servantErrorShow)

runClient :: BaseUrl -> (Either ClientError a -> action) -> ClientM a -> Transition action model ()
runClient url c route = scheduleIO $ c <$> runClientM route (ClientEnv url (const $ pure ()))

empty_ = div_ [] []

divKeyed_ :: Key -> [Attribute action] -> [View action] -> View action
divKeyed_ = node HTML "div" . pure

servantErrorShow (Left a) = Left (toMisoString $ show a)
servantErrorShow (Right x) = Right x

globalFragment model = toMisoString $ uriFragment $ modelURI $ model

controlledInput_ model props placeholder nr alternative action =
  input_ ([ placeholder_ placeholder
          , value_ (fromMaybe "" ((model ^? (#fields . ix nr)) <|> alternative))
          , onInput (localAction . action nr)
          ] ++ props)
