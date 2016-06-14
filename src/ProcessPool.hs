import System.Posix.Process(forkProcess, getAnyProcessStatus)
import System.Posix.Signals(signalProcess, sigKILL)
import System.Posix.Types(ProcessID)
import Network.Socket
import System.Exit(exitSuccess)
import Data.Maybe(Maybe(..))
import System.Posix.Signals(installHandler, Handler(..), sigINT)
import Control.Monad(forever, forM)

processPoolSize :: Int
processPoolSize = 5

main :: IO ()
main = do
  listeningSocket <- setupSocket 3001
  pids <- forM [1..processPoolSize] $ \num ->
    forkProcess (runServer num listeningSocket)
  installHandler sigINT (Catch (killChildren pids >> exitSuccess)) Nothing
  monitorProcesses

killChildren :: [ProcessID] -> IO ()
killChildren = mapM_ (signalProcess sigKILL)

monitorProcesses :: IO ()
monitorProcesses = do
  mpid <- getAnyProcessStatus True True
  case mpid of
      Just (pid, status) -> do
        putStrLn $ "Process" ++ show pid ++ " quit unexpectedly\n\n"
        monitorProcesses
      Nothing -> do
        putStrLn "No processes have exited\n"
        monitorProcesses

setupSocket :: PortNumber -> IO Socket
setupSocket port = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet port 0) >> listen s 5
  setSocketOption s ReuseAddr 1
  return s

runServer :: Int -> Socket -> IO ()
runServer num socket =
  forever $ do
    (connection, _) <- accept socket
    msg <- recv connection 1024
    send connection $ "Received Connection in process Number" ++ show num ++ "\n\n"
    send connection $ "\n Echo..." ++ msg
    close connection
