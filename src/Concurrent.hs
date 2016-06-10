module Concurrent(detachProcess) where
import System.Posix.Process(getProcessStatus)
import System.Posix.Types(ProcessID)
import Control.Concurrent(forkIO)

detachProcess :: ProcessID -> IO ()
detachProcess pid = do
                    forkIO $ do
                      getProcessStatus True True pid
                      return ()
                    return ()

