module HackBar.Clean.Web.Hooks.UseToiletCleaningProgress where

import Prelude

import Affjax as AX
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import HackBar.Clean.Types (ToiletCleaningProgress, toiletCleaningProgressDefault)
import HackBar.Clean.Web.Hooks.UseAffjax (AffjaxResult(..), UseAffjax, useAffjax)
import Halogen.Hooks (class HookNewtype, type (<>), HookType, UseState, useState)
import Halogen.Hooks as Hooks
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (readAsForeign, writeJSON)

type UseToiletCleaningProgress' = UseState ToiletCleaningProgress <> UseAffjax <> Hooks.Pure

foreign import data UseToiletCleaningProgress :: HookType

instance HookNewtype UseToiletCleaningProgress UseToiletCleaningProgress'

type ToiletCleaningProgressAPI m = 
  { value :: ToiletCleaningProgress
  , busy :: Boolean
  , isBusy :: Hooks.HookM m Boolean
  , fetch :: Hooks.HookM m ToiletCleaningProgress
  , save :: ToiletCleaningProgress -> Hooks.HookM m Unit
  }

useToiletCleaningProgress 
  :: forall m
   . MonadAff m 
  => Hooks.Hook m UseToiletCleaningProgress (ToiletCleaningProgressAPI m)
useToiletCleaningProgress = Hooks.wrap hook 
  where 
  hook :: Hooks.Hook _ UseToiletCleaningProgress' _
  hook = Hooks.do
    value /\ valueId <- useState toiletCleaningProgressDefault
    affjaxApi <- useAffjax
    let
      fetch :: Hooks.HookM m ToiletCleaningProgress
      fetch = do 
        resp <- affjaxApi.sendRequest GET "/api/cleaning-progress/toilet" Nothing
        case resp of
          Success json -> case readAsForeign json of 
            Right progress -> do 
              Hooks.put valueId progress 
              pure progress
            -- #FIXME エラー・ハンドリング
            Left _ -> unsafeCrashWith "Failed to decode response body json"
          AffjaxError err -> do
            Console.error (AX.printError err)
            unsafeCrashWith "Failed to send fetch request"
          _ -> do
            unsafeCrashWith "An error occurred during sending request"

      save :: ToiletCleaningProgress -> Hooks.HookM m Unit
      save progress = do
        resp <- affjaxApi.sendRequest PUT "/api/cleaning-progress/toilet" (Just $ writeJSON progress)
        case resp of 
          Success _ -> pure unit
          NoResponseBody -> pure unit
          -- #FIXME エラー・ハンドリング
          _ -> unsafeCrashWith "Failed to send request"

    Hooks.pure
      { value
      , busy: affjaxApi.loading
      , isBusy: affjaxApi.isLoading
      , fetch
      , save
      }