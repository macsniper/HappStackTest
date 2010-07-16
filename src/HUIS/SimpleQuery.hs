module HUIS.SimpleQuery where

import Text.XHtml.Transitional hiding (dir,content)
import HUIS.Database
import Database.HDBC
import Database.HDBC.ODBC
import Happstack.Server
import Control.Monad.Trans (MonadIO, liftIO)
import HUIS.StaticResponses
import Data.List
import Data.Char

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
  
-- | Generating HTML-Output from Query
queryToHtml:: [[SqlValue]]-> Html
queryToHtml stmt = thehtml <<
    [ header << headerContent "Einfaches Query-Interface"
    , body << concat [upperBody, simpleQueryForm, (resultTable stmt), lowerBody]]  


instance FromData SimpleQuery where
  fromData = do
    queryS <- look "queryselect"
    queryF <- look "queryfrom"
    queryW <- look "querywhere"
    let query = queryOnly $ "SELECT " ++ queryS ++ " FROM " ++ queryF ++ " WHERE " ++ queryW
    return SimpleQuery{content = query}

-- | Checks for bad keywords ("DROP", "DELETE", "INSERT", "UPDATE" etc)
queryOnly:: String-> String
queryOnly [] = []
queryOnly s =
  if [True] `isInfixOf` ( map (\st -> st `isInfixOf` (map toUpper s)) disallowed)
  then ""
  else s
  where disallowed = ["DROP", "DELETE", "INSERT", "UPDATE"]