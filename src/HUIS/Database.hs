module HUIS.Database (connectDatabase, selectQuery) where

import Database.HDBC
import Database.HDBC.ODBC
import HUIS.ConfigParser(Config)
import Data.Map


connectDatabase:: Config -> IO Connection
connectDatabase config = connectODBC $ "DSN=" ++  (config ! "dbsource") ++ ";UID" ++  (config ! "dbuser") ++ ";PWDDBMS" ++ (config ! "dbpass") ++ ";"

selectQuery:: String -> Connection -> IO [[SqlValue]]
selectQuery querystring conn = do
  rows <- quickQuery' conn querystring []
  return rows
  

