module NaiveHttpRequestParser(naiveHttpRequestParser, Request(..), RequestType(..)) where
import Data.List.Split(splitOn)
import Data.List(isPrefixOf)
import Data.List(takeWhile)

data RequestType = GET | POST deriving Show
data Request = Request { rType :: RequestType, rBody :: [(String, String)], path :: String, rawRequest :: String } deriving Show

requestType :: String -> Either String RequestType
requestType request
  | "GET" `isPrefixOf` request  = Right GET
  | "POST" `isPrefixOf` request = Right POST
  | otherwise                   = Left "Invalid request type"



naiveHttpRequestParser :: String -> Either String Request
naiveHttpRequestParser request = do
                            let path = requestPath request
                            case requestType request of
                              Right GET  -> Right (Request GET [] path request)
                              Right POST -> Right (Request POST (requestBody request) path request)
                              Left  err  -> Left err



requestPath :: String -> String
requestPath request = reverse $ takeWhile (/= '/') $ reverse $ takeWhile (not . isSpace) $ dropWhile (/= '/') request

isSpace :: Char -> Bool
isSpace = (==) ' '

requestBody :: String -> [(String, String)]
requestBody request = do
  [key, val] <- splitOn "=" <$> splitOn "&"(lines request !!7)
  return (key, val)


