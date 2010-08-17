module HUIS.SimpleQuery where

import Text.XHtml.Transitional hiding (dir,content)
import HUIS.Database
import Database.HDBC
import Database.HDBC.ODBC
import Happstack.Server
import Control.Monad.Trans (MonadIO, liftIO)
import HUIS.StaticResponses
import Data.List

data SimpleQuery = SimpleQuery{sel :: String,
                               from :: String,
                               whe :: String}

nullQuery = SimpleQuery{sel = "*", from = "", whe = "1=1"}

simpleQueryForm:: SimpleQuery -> [Html]
simpleQueryForm query = [
  thediv ! [theclass "Welcome"] << [
    [ p << "Einfaches Query-Interface, nur SELECT-Abfragen moeglich.",
      gui "/simplequery" <<
        [ table << [
          tr << [
            td ! [theclass "entry"] << stringToHtml "SELECT ",
            td << textfield "queryselect" ! [size "10", value (sel query)]
          ],
          tr << [
            td ! [theclass "entry"] << stringToHtml "FROM ",
            td << textfield "queryfrom" ! [size "20", value (from query)]
          ],
          tr << [
            td ! [theclass "entry"] << stringToHtml "WHERE ",
            td << textfield "querywhere" ! [size "20", value (whe query)]
          ]
         ]
        ]
      ]
    ]
  ]


simpleQueryResult:: Connection-> SimpleQuery-> ServerPart Response
simpleQueryResult conn req = do
  result <- liftIO $ handleSqlError $ quickQuery conn (buildQuery req) []
  queryToHtml result (simpleQueryForm req)


buildQuery:: SimpleQuery -> String
buildQuery a = "SELECT " ++ sel a ++ " FROM " ++ from a ++ " WHERE " ++ whe a ++ ";"


instance FromData SimpleQuery where
  fromData = do
    queryS <- look "queryselect"
    queryF <- look "queryfrom"
    queryW <- look "querywhere"
    return SimpleQuery{sel = queryOnly queryS, from = queryOnly queryF, whe = queryOnly queryW}

