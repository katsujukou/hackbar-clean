module HackBar.Clean.Web.Hooks.UseAffjax where

import Prelude

import Affjax as AX
import Affjax.RequestBody as ARQ
import Affjax.ResponseFormat as ARF
import Affjax.Web as AW
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.HTTP.Method (Method)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Foreign (Foreign, ForeignError(..), renderForeignError)
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookType, UseState, useState)
import Halogen.Hooks as Hooks
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (parseJSON, readAsForeign)


foreign import data UseAffjax :: HookType

type UseAffjax' = UseState Boolean <> Hooks.Pure

instance HookNewtype UseAffjax UseAffjax'

type URL = String 

data AffjaxResult 
  = AffjaxError AX.Error
  | ResponseDecodeError (NonEmptyList ForeignError)
  | NoResponseBody
  | Success Foreign

type AffjaxAPI m = 
  { loading :: Boolean
  , isLoading :: Hooks.HookM m Boolean
  , setLoading :: Boolean -> Hooks.HookM m Unit
  , sendRequest :: Method -> URL -> Maybe String -> Hooks.HookM m AffjaxResult
  }

useAffjax :: forall m. MonadAff m => Hook m UseAffjax (AffjaxAPI m)  
useAffjax = Hooks.wrap hook 
  where
  hook :: Hook _ UseAffjax' _
  hook = Hooks.do
    loading /\ loadingId <- useState false
    let 
      sendRequest :: Method -> String -> Maybe String -> Hooks.HookM m AffjaxResult 
      sendRequest method endpoint mbBody = do
        Hooks.put loadingId true
        resp <- liftAff $ AX.request AW.driver (AX.defaultRequest
          { method = Left method
          , url = endpoint
          , content = ARQ.String <$> mbBody
          , responseFormat = ARF.string
          })
        Hooks.put loadingId false
        case resp of 
          Left err -> pure $ AffjaxError err 
          Right { body }
            | "" <- body -> pure $ NoResponseBody
            | otherwise -> let _ = spy "body" body in case runExcept $ parseJSON body of 
                Left err -> do
                  Console.error $ foldMap renderForeignError err
                  pure $ ResponseDecodeError err 
                Right json -> pure $ Success json
    Hooks.pure 
      { loading 
      , isLoading: Hooks.get loadingId
      , setLoading: Hooks.put loadingId
      , sendRequest
      }