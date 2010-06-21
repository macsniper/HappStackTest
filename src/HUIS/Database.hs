module HUIS.Database (connectDatabase, selectQuery) where


import Database.HSQL
import Database.HSQL.ODBC
import Database.HSQL.Types
import HUIS.ConfigParser(Config)
import Data.Map


connectDatabase:: Config -> IO Connection
connectDatabase config = connect (config ! "dbsource") (config ! "dbuser") (config ! "dbpass")

selectQuery:: String -> Connection -> IO Statement
selectQuery querystring conn = do
  rows <- query conn querystring
  return rows
  

