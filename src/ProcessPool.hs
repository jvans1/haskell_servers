import System.Posix.Process(forkProcess, getAnyProcessStatus)
import System.Posix.Signals(signalProcess, sigKILL)
import System.Posix.Types(ProcessID)
import Network.Socket
import System.Exit(exitSuccess)
import Data.Maybe(Maybe(..))
import System.Posix.Signals(installHandler, Handler(..), sigINT)
import Control.Monad(forever, forM)
import Concurrent(detachProcess)
import TCPServer(setupSocket)

processPoolSize :: Int
processPoolSize = 5

main :: IO ()
main = do
  listeningSocket <- setupSocket 3001
  pids <- forM [1..processPoolSize] $ \num ->
    forkProcess (runServer num listeningSocket)
  installHandler sigINT (Catch (killChildren pids >> exitSuccess)) Nothing
  monitorProcesses listeningSocket

killChildren :: [ProcessID] -> IO ()
killChildren = mapM_ (signalProcess sigKILL)


monitorProcesses :: Socket -> IO ()
monitorProcesses listeningSocket = do
  mpid <- getAnyProcessStatus True True
  case mpid of
      Just (pid, status) -> do
        putStrLn $ "Process" ++ show pid ++ " quit unexpectedly\n\n"
        monitorProcesses listeningSocket
      Nothing -> do
        putStrLn "No processes have exited\n"
        monitorProcesses listeningSocket

runServer :: Int -> Socket -> IO ()
runServer num socket =
  forever $ do
    (connection, _) <- accept socket
    putStrLn $ "Processing Connection on server" ++ show num ++ "\n\n"
    send connection $ "Received Connection in process Number" ++ (show num)
    close connection
