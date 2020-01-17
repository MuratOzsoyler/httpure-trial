module Main where

import Prelude

import Control.Monad.Logger.Trans (class MonadLogger, info, runLoggerT)
import Control.Monad.Reader (class MonadAsk, ask, asks, runReaderT)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Message (Message)
import Data.Log.Tag (empty, fromArray, intTag, tag)
import Data.Map (toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import HTTPure (Request, Response, ResponseM, fullPath, notFound, ok, serve')
import Node.HTTP (ListenOptions)

type ServerEnv = { port :: Int }

type RequestEnv = { }

main :: Effect Unit
main = void $ runReaderT (runLoggerT bootServer serverLogger) serverEnv

bootServer :: forall m. MonadAsk ServerEnv m => MonadLogger m => MonadEffect m => m Unit
bootServer = void $ do
    serverOptions' <- serverOptions
    port <- asks _.port
    router' <- router
    liftEffect $ serve' serverOptions' router' (onServerStart port) -- router onStart

serverOptions :: forall m. MonadAsk ServerEnv m => m ListenOptions
serverOptions = 
    asks _.port 
        >>= pure 
        <<< { backlog: Nothing, hostname: "0.0.0.0", port: _ }

serverEnv :: ServerEnv
serverEnv = { port: 8080 }

serverLogger :: forall m. MonadEffect m => Message -> m Unit
serverLogger msg = liftEffect <<< Console.log =<< prettyFormatter msg

requestLogger :: forall m. MonadEffect m => Request -> Message -> m Unit
requestLogger request msg = 
    liftEffect 
        <<< Console.log 
        =<< prettyFormatter
        =<< pure inject
  where
    inject = msg 
        { tags = fromArray
            [ msg.tags
            , tag "method" (show request.method)
            , tag "http-version" (show request.httpVersion)
            , tag "url" (show $ fullPath request)
            , allHeaders
            ] 
        }
    allHeaders = fromArray
        $ map (\(Tuple k v) -> tag (unwrap k) v)
        $ toUnfoldable -- :: Map CaseInsensitiveString String -> Array (Tuple CaseInsensitiveString String))
        $ unwrap request.headers

router :: forall m. MonadAsk ServerEnv m => m (Request -> ResponseM)
router = do
    env <- ask
    pure \request -> do
        let response = routeRequest request
            requestLogger' = requestLogger request
        requestEnv env request >>= runReaderT (runLoggerT response requestLogger')

requestEnv :: forall m. Monad m => ServerEnv -> Request -> m RequestEnv
requestEnv env request = pure { }
    
routeRequest :: forall m. MonadLogger m => MonadAff m => Request -> m Response
routeRequest request = do
    info empty "Request started" 
    response <- case request of
        {path: ["hello"]} -> ok "Hello darling!"
        _ -> notFound
    info empty "Request started"
    pure response

onServerStart :: forall m. MonadEffect m => Int -> m Unit
onServerStart port = do
     runLoggerT (info (intTag "port" port) "Server started") serverLogger
    