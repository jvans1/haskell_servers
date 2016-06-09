import Network.Socket
import Data.Either(Either(..))
import Control.Monad.IO.Class(liftIO)
import Control.Applicative((<$>))
import Data.List.Split(splitOn)
import Data.List(isPrefixOf, partition)
import TCPServer
import Data.Map.Strict(empty, Map, insert)
data RequestType = GET | POST deriving Show
data Request = Request { rType :: RequestType, rBody :: [(String, String)] } deriving Show

main :: IO ()
main =
  tcpServerLoop 3001 empty $ \connection storage -> do
    erequest <- naiveHttpRequestParser <$> recv connection 4096
    newStorage <- case erequest of
      Right (Request GET _) -> do
       send connection (show storage)
       return storage
      Right (Request POST body) -> do
       let newStore = storeParams storage body
       send connection (show newStore)
       return newStore
      Left  err -> do
       send connection err
       return storage
    close connection >> return newStorage

storeParams ::Map String String -> [(String, String)] ->  Map String String
storeParams  = foldr (\(key, val) storage -> insert key val storage) 


naiveHttpRequestParser :: String -> Either String Request
naiveHttpRequestParser request = case requestType request of
                              Right GET  -> Right (Request GET [])
                              Right POST -> Right (Request POST (requestBody request))
                              Left  err  -> Left err

requestType :: String -> Either String RequestType
requestType request
  | "GET" `isPrefixOf` request  = Right GET
  | "POST" `isPrefixOf` request = Right POST
  | otherwise                   = Left "Invalid request type"


requestBody :: String -> [(String, String)]
requestBody request = do
  [key, val] <- splitOn "=" <$> splitOn "&"(lines request !!7)
  return (key, val)


