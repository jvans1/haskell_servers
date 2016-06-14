import TCPServer(setupSocket)
import Prelude hiding (lookup)
import Network.Socket
import NaiveHttpRequestParser(naiveHttpRequestParser, Request(..), RequestType(..))
import Control.Concurrent(forkIO, threadDelay, MVar, newMVar, takeMVar, putMVar)
import Control.Monad(forM_, forever)
import Data.Map.Strict(Map, empty, lookup, insert)

newtype Database = Database (MVar (Map String String) )

createDB :: IO Database
createDB = Database <$> newMVar empty

threadPoolSize :: Int
threadPoolSize = 5000

main :: IO ()
main = do
  listeningSocket <- setupSocket 3001
  db <- createDB
  forM_ [1..threadPoolSize] $ \num -> do
    putStrLn  $ "Forking thread " ++ show num
    forkIO (runServer db num listeningSocket)
  forever (threadDelay 10000000)

runServer :: Database -> Int -> Socket -> IO ()
runServer database num socket =
  forever $ do
    (connection, _) <- accept socket
    erequest <- naiveHttpRequestParser <$> recv connection 4096
    case erequest of
      Right (Request GET _ path _) -> do
       result <- runQuery path database
       send connection result
      Right (Request POST body _ _) -> do
       newStore <- storeParams database body
       send connection ("" ++ show newStore)
      Left  _ -> send connection "Invalid response"
    close connection

runQuery :: String -> Database -> IO String
runQuery path (Database mvar) = do
  db <- takeMVar mvar
  putMVar mvar db
  case lookup path db of
    Just value -> return $ "Retrieved Value: " ++ value
    Nothing ->  return $ "No Value stored at: " ++ path

storeParams ::Database -> [(String, String)] ->  IO ()
storeParams  (Database mvar) params = do 
  db <- takeMVar mvar
  let newDB = foldr (\(key, val) storage -> insert key val storage) db params 
  putMVar mvar newDB
