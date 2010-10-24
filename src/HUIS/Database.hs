module HUIS.Database (connectDatabase, selectQuery, stripchars, queryOnly, newFromSql, connectDatabaseReminder) where

import Database.HDBC
import Database.HDBC.ODBC
import Database.HDBC.Sqlite3
import HUIS.ConfigParser(Config)
import Data.Convertible.Base
import Data.Map hiding(map)
import Data.List(isInfixOf)
import Text.XHtml.Transitional hiding ((!))

import Data.Char


-- | Used to connect to the database.
connectDatabase:: Config -> IO Database.HDBC.ODBC.Connection
connectDatabase config = connectODBC $ "DSN=" ++  (config ! "dbsource") ++ ";UID=" ++  (config ! "dbuser") ++ ";PWD=" ++ (config ! "dbpass") ++";"

connectDatabaseReminder:: IO Database.HDBC.Sqlite3.Connection
connectDatabaseReminder = connectSqlite3 "database.sqlite"


-- | Executes a query.
selectQuery:: String -> Database.HDBC.ODBC.Connection -> IO [[SqlValue]]
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


newFromSql:: ConvertResult String -> String
newFromSql s =
  case s of
    Left b -> ""
    Right a -> a

-- | Checks for bad keywords ("DROP", "DELETE", "INSERT", "UPDATE" etc)
queryOnly:: String-> String
queryOnly [] = []
queryOnly s =
  if [True] `isInfixOf` ( map (\st -> st `isInfixOf` (map toUpper s)) disallowed)
  then ""
  else s
  where disallowed = ["DROP", "DELETE", "INSERT", "UPDATE"]

