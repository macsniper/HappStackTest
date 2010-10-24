module CsvOutput where

import Database.HDBC
import Database.HDBC.SqlValue
import HUIS.Birthday

-- | A hidden input field
hidden  :: Monad m => Maybe String -> XHtmlForm m String
hidden  =  input' X.hidden

csvForm:: [Html]
csvForm =
  [submit "run" "CSV-Ausgabe"]

-- Abfrage erstellen, wo die Datei gespeichert werden soll

birthdayCsv :: [[Sqlvalue]] -> String -> IO()
birthdayCsv BirtdayResultOhneHtml path = do
  writeFile path "Titel, Vorname(n), Zus., Nachname, Institut, Alter, Geburtsdatum"
  map CsvLine Statement path "/n"


CsvLine :: [Sqlvalue] -> String -> IO()
CsvLine resline path =
   map CsvValue resline path

CsvValue :: SqlValue -> String -> IO()
CsvValue val path = appendFile path fromsql $ val
  appendFile path ", "



