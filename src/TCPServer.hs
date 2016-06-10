module TCPServer where
import Network.Socket
import System.Exit(exitSuccess)
import System.Posix.Signals(installHandler, Handler(..), sigINT)
import Data.Map.Strict(Map)
type DataStore = Map String String

tcpServerLoop :: PortNumber -> DataStore -> (Socket -> DataStore -> IO DataStore) -> IO ()
tcpServerLoop port storage requestHandler = do
  s <- setupSocket port
  installHandler sigINT (Catch (close s >> exitSuccess)) Nothing
  loop s storage requestHandler
  return ()

setupSocket :: PortNumber -> IO Socket
setupSocket port = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet port 0) >> listen s 5
  setSocketOption s ReuseAddr 1
  return s


loop :: Socket -> Map String String -> (Socket -> DataStore -> IO DataStore) -> IO DataStore
loop socket storage requestHandler = do
  (connection, a) <- accept socket
  newStorage <- requestHandler connection storage
  loop socket newStorage requestHandler

