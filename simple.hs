import Text.Printf
import Network
import System.IO
import Control.Monad
import Control.Concurrent

handleLine :: String -> String
handleLine req = case words req of
  ["hello", arg] -> "hello " ++ (show.(+1).read) arg
  otherwise -> "unknown request"

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
