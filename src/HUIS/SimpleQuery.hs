module HUIS.SimpleQuery where

import Text.XHtml.Transitional hiding (dir,content)
import HUIS.Database
import Database.HDBC
import Database.HDBC.ODBC
import Happstack.Server
import Control.Monad.Trans (MonadIO, liftIO)
import HUIS.StaticResponses

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

simpleQueryResult:: Connection-> SimpleQuery-> ServerPart Response
simpleQueryResult conn req = do
  result <- liftIO $ handleSqlError $ quickQuery conn (content req) []
  ok $ toResponse $ queryToHtml result
  
queryToHtml:: [[SqlValue]]-> Html
queryToHtml stmt = thehtml <<
    [ header << headerContent "Einfaches Query-Interface"
    , body << concat [upperBody, simpleQueryForm, (resultTable stmt), lowerBody]]  


resultTable:: [[SqlValue]]-> [Html]
resultTable res = 
  [table << map resultLine res]
  
resultLine:: [SqlValue]-> Html
resultLine resline =
  tr << map resultEntry resline
  
resultEntry:: SqlValue-> Html
resultEntry val = td << (stringToHtml $ fromSql val)
  

instance FromData SimpleQuery where
  fromData = do
    queryS <- look "queryselect"
    queryF <- look "queryfrom"
    queryW <- look "querywhere"
    let query = "SELECT " ++ queryS ++ " FROM " ++ queryF ++ " WHERE " ++ queryW
    return SimpleQuery{content = query}