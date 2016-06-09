import Network.Socket
import Prelude hiding (lookup)
import Data.Either(Either(..))
import NaiveHttpRequestParser(naiveHttpRequestParser, Request(..), RequestType(..))
import Control.Applicative((<$>))
import TCPServer
import Data.Map.Strict(empty, Map, insert, lookup)

main :: IO ()
main =
  tcpServerLoop 3001 empty $ \connection storage -> do
    erequest <- naiveHttpRequestParser <$> recv connection 4096
    newStorage <- case erequest of
      Right (Request GET _ path _) -> do
       send connection $ maybe ("No Value stored at: " ++ path) ((++) "Retrieved Value: ") (lookup path storage)
       return storage
      Right (Request POST body _ _) -> do
       let newStore = storeParams storage body
       send connection (show newStore)
       return newStore
      Left  _ -> do
       send connection "Invalid response"
       return storage
    close connection >> return newStorage

storeParams ::Map String String -> [(String, String)] ->  Map String String
storeParams  = foldr (\(key, val) storage -> insert key val storage) 

