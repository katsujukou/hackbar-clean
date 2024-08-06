module HackBar.Clean.Web.Capability where

import Prelude

import Control.Monad.Trans.Class (lift)
import Effect.Aff.Class (class MonadAff)
import HackBar.Clean.Types (ToiletCleaningProgress)
import Halogen (HalogenM)
import Halogen.Hooks (HookM)


class MonadAff m <= MonadToiletCleaningProgress m where
  getToiletCleaningProgress :: m ToiletCleaningProgress
  putToiletCleaningProgress :: ToiletCleaningProgress -> m Unit

instance MonadToiletCleaningProgress m => MonadToiletCleaningProgress (HookM m) where
  getToiletCleaningProgress = lift getToiletCleaningProgress 
  putToiletCleaningProgress = putToiletCleaningProgress >>> lift

instance MonadToiletCleaningProgress m => MonadToiletCleaningProgress (HalogenM st act slo o m) where
  getToiletCleaningProgress = lift getToiletCleaningProgress 
  putToiletCleaningProgress = putToiletCleaningProgress >>> lift
