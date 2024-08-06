module HackBar.Clean.Web.Component.HeaderMenu where

import Prelude

import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

make :: forall q i o m. H.Component q i o m 
make = Hooks.component \_ _ -> Hooks.do
  Hooks.pure do
    HH.div 
      [ HP.class_ $ ClassName "w-full p-3 font-bold text-lg text-white bg-black" ]
      [ HH.text "ハックバーそうじ進捗共有システム（やっつけ版）" ]