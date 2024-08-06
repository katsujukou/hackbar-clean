module HackBar.Clean.Web where

import Prelude

import Effect (Effect)
import HackBar.Clean.Web.App (app)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI app {} body
