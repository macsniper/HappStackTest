module HUIS.Personensuche where

import Text.ParserCombinators.Parsec hiding (spaces, label)
import Text.XHtml.Transitional hiding (dir, content)
import Data.Time (formatTime, getCurrentTime, UTCTime)
import Control.Monad.Trans (MonadIO, liftIO)
import Happstack.Server
import HUIS.Database
import Database.HDBC
import Database.HDBC.ODBC
import HUIS.StaticResponses
import Data.List
import Data.Char
import HUIS.WhereQueryErmittlung(whereQueryZu)

type SQL = String
type ErgebnisTabelle = [[SqlValue]]

newtype Personensuche = Personensuche{ content:: SQL}

personensucheForm:: [Html]
personensucheForm =
  [ gui "/Personensuche" <<
    [ label << "PersonenId: "
    , textfield "personenid" ! [size "30", value ""]
    , label << " Vorname Nachname: "
    , textfield "gesamtName" ! [size "30", value ""]
    , submit "run" "Suche Person" ]
  , thediv << "HUIS-Personensuche" ]

personensucheResult:: Connection-> String -> ServerPart Response
personensucheResult conn req = do
  let query= "SELECT pgd_vorname, pgd_name FROM pgd" ++ req
  result <- liftIO $ handleSqlError $ quickQuery conn query []
  queryToHtml [] result personensucheForm  

instance FromData Personensuche where
 fromData = do
    gesamtName <- look "gesamtName"
    let temp = whereQueryZu gesamtName
    return Personensuche{content = temp}


-- ergebnis abfangen
-- parser aufrufen
-- ergebnis übergeben

