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

newtype Personensuche = Personensuche{ content:: SQL }

personensucheForm:: [Html]
personensucheForm =
  [ gui "/Personensuche" <<
    [ label << "PersonenId: "
    , textfield "personenid" ! [size "30", value ""]
    , label << " Vorname Nachname: "
    , textfield "gesamtName" ! [size "30", value ""]
    , submit "run" "Suche Person" ]
  , thediv << "HUIS-Personensuche" ]

personensucheResult:: Connection-> Personensuche -> ServerPart Response
personensucheResult conn req = do
  result <- liftIO $ handleSqlError $ quickQuery conn (content req) []
  ok $ toResponse $ queryyToHtml result


-- | Generating HTML-Output from Query
queryyToHtml:: ErgebnisTabelle-> Html
queryyToHtml stmt = thehtml <<
    [ header << headerContent "HUIS-Personensuche"
    , body << concat [upperBody, personensucheForm, resultTable stmt, lowerBody]]

instance FromData Personensuche where
 fromData = do
    personenid <- look "personenid"
    gesamtName <- look "gesamtName"
    let query= "personenid " ++ personenid ++ " gesamtName " ++ gesamtName
    return Personensuche{content = query}



