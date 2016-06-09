module TCPServer where
import Network.Socket

tcpServerLoop :: PortNumber -> (Socket -> IO ()) -> IO ()
tcpServerLoop port requestHandler = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet port 0) >> listen s 5
  loop s requestHandler


loop :: Socket -> (Socket -> IO ()) -> IO ()
loop socket requestHandler = do
  (connection, a) <- accept socket
  requestHandler connection
  loop socket requestHandler
