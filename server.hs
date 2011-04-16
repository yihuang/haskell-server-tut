{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.IO
import Data.Maybe
import Text.Printf
import Network
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Concurrent

data Request = Hello Int
             | Stat
  deriving (Show, Read)

data Reponse = HelloRep Int
             | StatRep Env
  deriving (Show, Read)

data Env = Env {
  clients :: Int,
  requests :: Int
} deriving (Show, Read)

newtype ServerMT m a = ServerMT (StateT Env m a)
  deriving (Functor, Monad, MonadState Env)

runServerMT :: Env -> ServerMT m a -> m (a, Env)
runServerMT env (ServerMT m) = runStateT m env

maybeRead = fmap fst . listToMaybe . reads

handleReq :: (Monad m, Functor m) => Request -> ServerMT m Reponse
handleReq req = case req of
  (Hello seq) -> return . HelloRep $ seq+1
  Stat        -> get >>= return . StatRep

handleLine :: (Monad m, Functor m) => String -> ServerMT m String
handleLine req = case maybeRead req of
  Just req -> fmap show . handleReq $ req
  Nothing  -> return "bad request"

handleClient :: (Handle, HostName, PortNumber) -> MVar Env -> IO ()
handleClient (csock, chost, cport) env = do
  modifyMVar_ env $ \env -> return env{clients=(clients env)+1}
  forever $ do
    req <- hGetLine csock
    putStrLn $ printf "recved %s from %s" req (show cport)
    threadDelay (1000*1000*1)
    modifyMVar_ env $ \env -> do
      (rep, env) <- runServerMT env . handleLine $ req
      hPutStrLn csock rep
      putStrLn $ printf "sended %s to %s" rep (show cport)
      return env{ requests=(requests env)+1 }

main = do
  withSocketsDo $ do
    let port = 8000
    env <- newMVar Env {
      clients=0,
      requests=0
    }
    sock <- listenOn (PortNumber port)
    putStrLn $ printf "listening on %s" (show port)
    forever $ do
      (csock, chost, cport) <- accept sock
      hSetBuffering csock NoBuffering
      forkIO $ handleClient (csock, chost, cport) env
