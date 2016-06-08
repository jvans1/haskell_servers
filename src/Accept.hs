import Network.Socket

main :: IO ()
main = do
  --sockets need to be assigned an address and a port number
  --AF_INET means we're using IPv4 addresses with this socket
  --Stream says that the socket is communicating
  --via a stream which (basically means we're TCP). For a UDP socket we would
  --have used Datagram
  s <- socket AF_INET Stream defaultProtocol
  -- bind the socket to port 3001
  -- the second argument to SockAddrInet (0) says
  -- to listen on all interfaces. We could
  -- have assigned 127.0.0.1 but then any request
  -- made not from localhost would not connect 
  -- to this socket
  bind s (SockAddrInet 3001 0) 
  --listen with a 'listen queue' size of 5. 
  --After 5 connections are queued, the server
  --will start refusing requests
  listen s 5
  --wait for a request, `accept` is blocking the thread here 
  --so no more work can be done
  --After a connection is "acceptED" it returns
  --a new socket, freeing up the server to
  --accept more connections. This new socket
  --This new socket is where the web application will
  --write data that will eventually be sent to the client
  (connectionSocket, a) <- accept s
  -- Write data to the socket
  send connectionSocket "HELLO WORLD!"
  --Cleanup
  close connectionSocket
  close s
  return ()
