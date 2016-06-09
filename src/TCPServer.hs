module TCPServer where
import Network.Socket
import Data.Map.Strict(Map)
type DataStore = Map String String

tcpServerLoop :: PortNumber -> DataStore -> (Socket -> DataStore -> IO DataStore) -> IO ()
tcpServerLoop port storage requestHandler = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet port 0) >> listen s 5
  loop s storage requestHandler 
  return ()


loop :: Socket -> Map String String -> (Socket -> DataStore -> IO DataStore) -> IO DataStore
loop socket storage requestHandler = do
  (connection, a) <- accept socket
  newStorage <- requestHandler connection storage
  loop socket newStorage requestHandler
