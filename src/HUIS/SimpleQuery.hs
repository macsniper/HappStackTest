module HUIS.SimpleQuery where

import Text.XHtml.Transitional hiding (dir,content)
import HUIS.Database
import Database.HDBC
import Database.HDBC.ODBC
import Happstack.Server

data SimpleQuery = SimpleQuery{content :: String}

simpleQueryForm:: [Html]
simpleQueryForm = 
  [ gui "/simplequery" << 
    [ label << "SELECT "
    , textfield "queryselect" ! [size "10", value "*"]
    , label << "FROM"
    , textfield "queryfrom" ! [size "20", value "table"]
    , label << "WHERE"
    , textfield "querywhere" ! [size "20", value "attribute"]
    , submit "run" "starten" ]
  , thediv << "Einfaches Query-Interface, nur SELECT-Abfragen moeglich."
  ]



  
simpleQueryResult:: Connection-> SimpleQuery-> [Html]
simpleQueryResult conn req =
  [thediv << ("Query-String war " ++ content req)]
  

  
  
instance FromData SimpleQuery where
  fromData = do
    queryS <- look "queryselect"
    queryF <- look "queryfrom"
    queryW <- look "querywhere"
    let query = "SELECT " ++ queryS ++ " FROM " ++ queryF ++ " WHERE " ++ queryW
    return SimpleQuery{content = query}