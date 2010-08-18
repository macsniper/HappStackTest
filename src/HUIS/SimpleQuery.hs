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
simpleQueryForm query =
   [ gui "/simplequery" <<
        [
      table ! [width "100%"] << [
        tr << [
          td ! [colspan 2, align "center"] << h2 << "Einfaches Datenbankabfrage-Formular"
        ],
        tr << [
          td ! [colspan 2, align "center"] << thediv ! [theclass "bg"] << noHtml
        ],
        tr << [
          td ! [thestyle "width: 115px;"] << label << "SELECT ",
          td ! [thestyle "width: 115px;"] << textfield "queryselect" ! [size "10", value (sel query)]
        ],tr << [
          td ! [thestyle "width: 115px;"] << label << "FROM ",
          td ! [thestyle "width: 115px;"] << textfield "queryfrom" ! [size "20", value (from query)]
        ], tr << [
          td ! [thestyle "width: 115px;"] << label << "WHERE",
          td ! [thestyle "width: 115px;"] << textfield "querywhere" ! [size "20", value (whe query)]
        ],tr << [
          td << noHtml,
          td << submit "run" "ausführen"
        ]
      ]
    ]
  ]


simpleQueryResult:: Connection-> SimpleQuery-> ServerPart Response
simpleQueryResult conn req = do
  stmt <- liftIO $ handleSqlError $ prepare conn (buildQuery req)
  liftIO $ handleSqlError $ execute stmt []
  result <- liftIO $ handleSqlError $ fetchAllRows stmt
  columns <- liftIO $ handleSqlError $ getColumnNames stmt
  queryToHtml columns result (simpleQueryForm req)


buildQuery:: SimpleQuery -> String
buildQuery a = "SELECT " ++ sel a ++ " FROM " ++ from a ++ " WHERE " ++ whe a ++ ";"


instance FromData SimpleQuery where
  fromData = do
    queryS <- look "queryselect"
    queryF <- look "queryfrom"
    queryW <- look "querywhere"
    return SimpleQuery{sel = queryOnly queryS, from = queryOnly queryF, whe = queryOnly queryW}

