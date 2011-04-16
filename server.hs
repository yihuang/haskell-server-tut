import System.IO
import Data.Maybe
import Text.Printf
import Network
import Control.Monad
import Control.Concurrent

data Request = Hello Int
  deriving (Show, Read)

maybeRead = fmap fst . listToMaybe . reads

handleLine :: String -> String
handleLine req = case maybeRead req of
  Just (Hello seq) -> show . Hello $ seq+1
  _ -> "bad request"

handleClient :: Handle -> HostName -> PortNumber -> IO ()
handleClient csock chost cport = forever $ do
  req <- hGetLine csock
  putStrLn $ printf "recved %s from %s" req (show cport)
  threadDelay (1000*1000*1)
  let rep = handleLine req
  hPutStrLn csock rep
  putStrLn $ printf "sended %s to %s" rep (show cport)

main = do
  withSocketsDo $ do
    let port = 8000
    sock <- listenOn (PortNumber port)
    putStrLn $ printf "listening on %s" (show port)
    forever $ do
      (csock, chost, cport) <- accept sock
      hSetBuffering csock NoBuffering
      forkIO $ handleClient csock chost cport
