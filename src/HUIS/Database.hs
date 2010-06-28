module HUIS.Database (connectDatabase, selectQuery, stripchars) where

import Database.HDBC
import Database.HDBC.ODBC
import HUIS.ConfigParser(Config)
import Data.Map
import Data.List

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