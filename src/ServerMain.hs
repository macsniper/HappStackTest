module ServerMain where

import Happstack.Server
import Text.XHtml.Transitional hiding (dir)
import Control.Monad (msum)

main = simpleHTTP (Conf 8080 Nothing) (testapp)

testapp:: ServerPart Response
testapp = msum 
  [ methodOnly GET >> path (handleRequest)
  , methodOnly POST >> withData (showRequest) ]

handleRequest:: String-> ServerPart Response
handleRequest test =
  ok $ toResponse $ showTestContent test

showTestContent:: String-> Html
showTestContent t = gui "/" << 
  [label << t
  , br
  , textfield "epicwin" ! [size "50", value $ t]
  , br
  , submit "update" "gogogo"]
  
showRequest:: String-> ServerPart Response
showRequest test =
  ok $ toResponse $ body << 
    [ stringToHtml test
    , br
    , anchor ! [href "/epicwin" ] << "back"]