import Network.Socket
import TCPServer
import Data.Map.Strict(empty)


main :: IO ()
main = tcpServerLoop 3001 empty $ \connection storage -> do
          msg <- recv connection 4096
          send connection (msg ++ ", Hello world!")
          close connection
          return storage


