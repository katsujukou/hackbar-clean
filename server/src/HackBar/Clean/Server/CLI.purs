module HackBar.Clean.Server.CLI where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import HackBar.Clean.Server (startServer)

main :: Effect Unit
main = do
  cb <- startServer
  cb do
    Console.log "Hew!"