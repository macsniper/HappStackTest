module HUIS.Database (connectDatabase, selectQuery, stripchars, resultTable) where

import Database.HDBC
import Database.HDBC.ODBC
import HUIS.ConfigParser(Config)
import Data.Map hiding(map)
import Data.List(isInfixOf)
import Text.XHtml.Transitional hiding ((!))


-- | Used to connect to the database.
connectDatabase:: Config -> IO Connection
connectDatabase config = connectODBC $ "DSN=" ++  (config ! "dbsource") ++ ";UID=" ++  (config ! "dbuser") ++ ";PWD=" ++ (config ! "dbpass") ++";"

-- | Executes a query.
selectQuery:: String -> Connection -> IO [[SqlValue]]
selectQuery querystring conn = do
  rows <- quickQuery' conn querystring []
  return rows
  
-- | Removes dangerous chars from a string.  
stripchars:: String-> String
stripchars [] = []
stripchars (x:xs) = 
  if [x] `isInfixOf` blocks
  then stripchars xs
  else x : stripchars xs
  where blocks = ['/','"','`',';', '\'']
  

resultTable:: [[SqlValue]]-> [Html]
resultTable res = 
  [table << map resultLine res]
  
resultLine:: [SqlValue]-> Html
resultLine resline =
  tr << map resultEntry resline
  
resultEntry:: SqlValue-> Html
resultEntry val = td << (stringToHtml $ fromSql val)
  