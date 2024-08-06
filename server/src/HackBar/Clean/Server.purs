module HackBar.Clean.Server where

import Prelude hiding ((/))

import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Effect.Aff as Exn
import Effect.Class.Console as Console
import Foreign (renderForeignError)
import HTTPurple (Method(..), ServerM, badRequest, jsonHeaders, mkRoute, noArgs, noContent, notFound, ok', root, serve, toString)
import HackBar.Clean.Types (ToiletCleaningProgress, toiletCleaningProgressDefault)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex.Generic (sumPrefix)
import Routing.Duplex.Generic.Syntax ((/))
import Simple.JSON (class ReadForeign, class WriteForeign, parseJSON, readAsForeign, writeJSON)
import Unsafe.Coerce (unsafeCoerce)

data ApiRoute = ToiletProgress 

derive instance Generic ApiRoute _ 
derive instance Eq ApiRoute
derive instance Ord ApiRoute 
instance Show ApiRoute where
  show = genericShow

apiRoute :: RouteDuplex' ApiRoute
apiRoute = mkRoute
  { "ToiletProgress": "cleaning-progress" / "toilet" / noArgs
  }

readOrCreate 
  :: forall a
   . ReadForeign a 
  => WriteForeign a 
  => FilePath 
  -> a
  -> Aff a 
readOrCreate filepath def = do
  try (readTextFile UTF8 filepath) >>= case _ of
    Left err -> do
      Console.error (Exn.message err)
      writeTextFile UTF8 filepath (writeJSON def)
      pure def
    Right content -> do
      case readAsForeign =<< runExcept (parseJSON content) of 
        Right a -> pure a
        Left _ -> unsafeCoerce "ho"

startServer :: ServerM 
startServer = do
  serve { port: 3000 } { route: apiRoute, router }

  where
  router { route, method, body } = case method, route of
    Get, ToiletProgress -> do
      progress <- readOrCreate "toilet.json" toiletCleaningProgressDefault
      ok' jsonHeaders $ writeJSON progress
    
    Put, ToiletProgress -> do
      str <- toString body

      let 
        parseToiletCleaningProgress :: String -> Either _ ToiletCleaningProgress
        parseToiletCleaningProgress = (parseJSON >>> runExcept) >=> readAsForeign
      
      case parseToiletCleaningProgress str of
        Right a -> do 
          Console.logShow a
          writeTextFile UTF8 "toilet.json" (writeJSON a)
          noContent
        Left err -> do
          Console.error $ foldMap renderForeignError err
          badRequest "Failed to parse json request."
        
    _, _ -> do
      notFound