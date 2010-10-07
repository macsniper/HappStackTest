module HUIS.CsvOutput where

import Database.HDBC
import Database.HDBC.SqlValue

-- | A hidden input field
hidden  :: Monad m => Maybe String -> XHtmlForm m String
hidden  =  input' X.hidden

csvForm:: [Html]
csvForm =
  [submit "run" "CSV-Ausgabe"]

-- Abfrage erstellen, wo die Datei gespeichert werden soll

birthdayCsv :: [[Sqlvalue]] -> String -> IO()
birthdayCsv Statement path = do
  writeFile path "Titel, Vorname(n), Zus., Nachname, Institut, Alter, Geburtsdatum"
  map CsvLine Statement path "/n"


csvLine :: [Sqlvalue] -> String -> IO()
csvLine resline path =
   map CsvValue resline path

csvValue :: SqlValue -> String -> IO()
csvValue val path = appendFile path fromsql $ val
  appendFile path ","



