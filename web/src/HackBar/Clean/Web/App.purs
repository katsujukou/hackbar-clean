module HackBar.Clean.Web.App where

import Prelude

import Effect.Aff.Class (class MonadAff)
import HackBar.Clean.Web.Component.HeaderMenu as HeaderMenu
import HackBar.Clean.Web.Views.ToiletPage as ToiletPage
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Type.Proxy (Proxy(..))

app :: forall q i o m. MonadAff m => H.Component q i o m 
app = Hooks.component \_ _ -> Hooks.do
  Hooks.pure do
    HH.div [ HP.class_ $ ClassName ""]
      [ HH.slot_ (Proxy@"header-menu") unit HeaderMenu.make {}
      , renderRouterView {}
      ]
  
  where
    renderRouterView = case _ of 
      _ -> HH.slot_ (Proxy@"toilet-view") unit ToiletPage.make {}