import Network.Socket
import TCPServer

main :: IO ()
main = tcpServerLoop 3001 $ \connection -> do
          msg <- recv connection 4096
          send connection (msg ++ ", Hello world!")
          close connection


