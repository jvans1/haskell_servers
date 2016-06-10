import Network.Socket
import System.Posix.Process(forkProcess, getProcessStatus)
import Control.Concurrent(threadDelay)
import Concurrent(detachProcess)

main :: IO ()
main = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet 3001 0) >> listen s 5
  handleRequest s
  close s

handleRequest :: Socket -> IO ()
handleRequest listeningSocket = do
    (connection, _) <- accept listeningSocket
    pid <- forkProcess $ do
      putStrLn "Process new Request"
      send connection "Hi, sleeping for 5 seconds...\n\nt"
      threadDelay 5000000
      send connection "Done Sleeping\n\n"
      close connection

    close connection
    detachProcess pid
    handleRequest listeningSocket


