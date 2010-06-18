module HUIS.SimpleQuery where

import Text.XHtml.Transitional hiding (dir,content)
import Database.HSQL
import Happstack.Server

data SimpleQuery = SimpleQuery{content :: String}

simpleQueryForm:: [Html]
simpleQueryForm = 
  [ gui "/simplequery" << 
    [ label << "SELECT "
    , textfield "querytext" ! [size "50", value "* FROM table WHERE TRUE"]
    , submit "run" "starten" ]
  , thediv << "Einfaches Query-Interface, nur SELECT-Abfragen moeglich."
  ]
  
simpleQueryResult:: Connection-> SimpleQuery-> [Html]
simpleQueryResult conn req =
  [thediv << ("Query-String war " ++ content req)]
  
instance FromData SimpleQuery where
  fromData = do
    query <- look "querytext"
    return SimpleQuery{content = query}