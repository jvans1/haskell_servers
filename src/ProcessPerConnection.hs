import Network.Socket
import System.Posix.Process(forkProcess)
import Control.Monad(forever)

main :: IO ()
main = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet 3001 0) >> listen s 5
  handleRequest s
  close s

{- handleRequest :: Socket ->  -}
handleRequest listeningSocket = do
    (connection, _) <- accept listeningSocket
    forkProcess $ do
      send connection "HELLO WORLD!"
      close connection
    handleRequest listeningSocket
