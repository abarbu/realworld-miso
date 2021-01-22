{-# LANGUAGE TypeFamilies, TypeApplications, ScopedTypeVariables, PartialTypeSignatures #-}
{-# OPTIONS -fno-warn-partial-type-signatures #-}
module Routes where
import           Miso hiding (runRoute)
import           Miso.String
import           Servant.API
import           Servant.Links
import           Misc
import           Data.JSString.Text
import qualified Api as A
import Data.Proxy
import Utils.Miso.Router

-- | Home page (URL: /#/ )
--  List of tags
--  List of articles pulled from either Feed, Global, or by Tag
--  Pagination for list of articles
-- Sign in/Sign up pages (URL: /#/login, /#/register )
--  Uses JWT (store the token in localStorage)
--  Authentication can be easily switched to session/cookie based
-- Settings page (URL: /#/settings )
-- Editor page to create/edit articles (URL: /#/editor, /#/editor/article-slug-here )
-- Article page (URL: /#/article/article-slug-here )
--  Delete article button (only shown to article's author)
--  Render markdown from server client side
--  Comments section at bottom of page
--  Delete comment button (only shown to comment's author)
-- Profile page (URL: /#/profile/:username, /#/profile/:username/favorites )
--  Show basic user info
--  List of articles populated from author's created articles or author's favorited articles

-- This API is lifted to break a module cycle.
--  Every page needs access to the routes so it can generate safe links
--  Types needs access to every page so that it can create the Model/Action for the app
--  Routes needs access to Types so that the API returns the correct actions

type API a  = Entry a :<|>
              Home a :<|>
              Login a :<|>
              Register a :<|>
              Settings a :<|>
              Article a :<|>
              Editor a :<|>
              ArticleEditor a :<|>
              Profile a :<|>
              ProfileFavorites a

type Entry a            = View a
type Home a             = "#" :> View a
type Login a            = "#" :> "login" :> View a
type Register a         = "#" :> "register" :> View a
type Settings a         = "#" :> "settings" :> View a
type Article a          = "#" :> "article" :> Capture "slug" MisoString :> View a
type Editor a           = "#" :> "editor" :> View a
type ArticleEditor a    = "#" :> "editor" :> Capture "slug" MisoString :> View a
type Profile a          = "#" :> "profile" :> Capture "username" MisoString :> View a
type ProfileFavorites a = "#" :> "profile" :> Capture "username" MisoString :> "favorites" :> View a

goto api d f = ChangeURI $ fixupFragmentLink $ linkURI $ f $ safeLink api d

linkto api d f = toMisoString $ uriFragment $ fixupFragmentLink $ linkURI $ f $ safeLink api d

fixupFragmentLink l = case uriPath l of
                        '%':'2':'3':rest -> l { uriPath = "", uriFragment = '#':rest }
                        _ -> l

-- * Safe link handling

goHome             :: forall a. _
goHome                      = goto (Proxy @(API a)) (Proxy @(Home a)) id
goLogin            :: forall a. _
goLogin                     = goto (Proxy @(API a)) (Proxy @(Login a)) id
goRegister         :: forall a. _
goRegister                  = goto (Proxy @(API a)) (Proxy @(Register a)) id
goSettings         :: forall a. _
goSettings                  = goto (Proxy @(API a)) (Proxy @(Settings a)) id
goArticle          :: forall a. _
goArticle slug              = goto (Proxy @(API a)) (Proxy @(Article a)) (\f -> f slug)
goEditor           :: forall a. _
goEditor                    = goto (Proxy @(API a)) (Proxy @(Editor a)) id
goArticleEditor    :: forall a. _
goArticleEditor slug        = goto (Proxy @(API a)) (Proxy @(ArticleEditor a)) (\f -> f slug)
goProfile          :: forall a. _
goProfile username          = goto (Proxy @(API a)) (Proxy @(Profile a)) (\f -> f username)
goProfileFavorites :: forall a. _
goProfileFavorites username = goto (Proxy @(API a)) (Proxy @(ProfileFavorites a)) (\f -> f username)

linkHome             :: forall a. _
linkHome                      = linkto (Proxy @(API a)) (Proxy @(Home a)) id
linkLogin            :: forall a. _
linkLogin                     = linkto (Proxy @(API a)) (Proxy @(Login a)) id
linkRegister         :: forall a. _
linkRegister                  = linkto (Proxy @(API a)) (Proxy @(Register a)) id
linkSettings         :: forall a. _
linkSettings                  = linkto (Proxy @(API a)) (Proxy @(Settings a)) id
linkArticle          :: forall a. _
linkArticle slug              = linkto (Proxy @(API a)) (Proxy @(Article a)) (\f -> f slug)
linkEditor           :: forall a. _
linkEditor                    = linkto (Proxy @(API a)) (Proxy @(Editor a)) id
linkArticleEditor    :: forall a. _
linkArticleEditor slug        = linkto (Proxy @(API a)) (Proxy @(ArticleEditor a)) (\f -> f slug)
linkProfile          :: forall a. _
linkProfile username          = linkto (Proxy @(API a)) (Proxy @(Profile a)) (\f -> f username)
linkProfileFavorites :: forall a. _
linkProfileFavorites username = linkto (Proxy @(API a)) (Proxy @(ProfileFavorites a)) (\f -> f username)
