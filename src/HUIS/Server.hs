module HUIS.Server where


import HUIS.Dispatcher
import HUIS.ConfigParser
import Happstack.Server
import Data.ConfigFile
import Data.Either.Utils
import Text.Regex
import Data.Map




main = do
  conf <- readConfig "huis.conf"
  port <- parseConfigFile "port"
  
  -- todo: put conf into single Map
  simpleHTTP (Conf (read port::Int) Nothing) (runDispatcher)

parseConfigFile:: String-> IO String
parseConfigFile name = do
  -- IO stuff :(
  val <- readfile emptyCP "huis.conf"
  let confparser = forceEither val
      initialvalue = show ((forceEither $ get confparser "DEFAULT" name):: String)
      -- do some regex-replacement-stuff (strip "")
      actualvalue = subRegex (mkRegex "\"") initialvalue ""
  return (actualvalue)

-- outdated code
{-
testapp:: ServerPart Response
testapp = msum 
  [ methodOnly GET  >> nullDir >> handleRequest "your Message!"
  , methodOnly POST >> withData (showRequest) 
  , methodOnly GET  >> path (handleRequest)]

handleRequest:: String-> ServerPart Response
handleRequest test =
  ok $ toResponse $ showTestContent test

showTestContent:: String-> Html
showTestContent t = gui "/" << 
  [label << t
  , br
  , textfield "epicfail" ! [size "50", value $ t]
  , br
  , submit "update" "gogogo"]
  
showRequest:: String-> ServerPart Response
showRequest test =
  ok $ toResponse $ body << 
    [ stringToHtml test
    , br
    , anchor ! [href "/" ] << "back"]
-}