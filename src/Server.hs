{-# LANGUAGE OverloadedStrings #-}
module Server
    ( start
    ) where
import Network.Socket

start :: IO ()
start = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet 3001 0) 
  listen s 5
  (connectionSocket, _) <- accept s
  send connectionSocket "HELLO WORLD!"
  close connectionSocket
  close s
  return ()
