import Network.Socket
import Control.Monad(forever)

main :: IO ()
main = do
  --Open the server socket only once
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet 3001 0) 
  listen s 5
  forever $ do
    --Once a connection is received process the connection and close it
    (connectionSocket, _) <- accept s
    send connectionSocket "HELLO WORLD!"
    close connectionSocket
  close s
