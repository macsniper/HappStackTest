module HUIS.Server where


import HUIS.Dispatcher
import Happstack.Server


main = simpleHTTP (Conf 8080 Nothing) (runDispatcher)

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