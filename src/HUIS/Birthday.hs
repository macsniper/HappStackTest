module HUIS.Birthday where

import Text.XHtml.Transitional hiding (dir,content)
import Happstack.Server
import HUIS.Database
import HUIS.StaticResponses
import Database.HDBC
import Database.HDBC.ODBC
import Control.Monad.Trans (MonadIO, liftIO)


data DateRange = DateRange{from :: String,
                           to :: String}

instance FromData DateRange where
  fromData = do
  --regex zur datumsüberprüfung^[0-9]{4}-(((0[13578]|(10|12))-(0[1-9]|[1-2][0-9]|3[0-1]))|(02-(0[1-9]|[1-2][0-9]))|((0[469]|11)-(0[1-9]|[1-2][0-9]|30)))$
  dateBeginn <- look "date1"
  dateEnd <- look "date2"
  return $ DateRange{from = dateBeginn, to = dateEnd}


birthdayForm:: [Html]
birthdayForm =
  [ gui "/birthday" <<
    [ label << "Date start"
    , textfield "date1" ! [size "30", value "'yyyy-mm-dd'"]
    , label << "Date end"
    , textfield "date2" ! [size "30", value "'yyyy-mm-dd'"]
    --, label << "Number of years"
    --, textfield "years" ! [size "30", value "1"]
    , submit "run" "starten" ]
  , thediv << "HUIS-Birthday"
  ]

birthdayResult:: Connection-> DateRange-> ServerPart Response
birthdayResult conn temp = do
  let querystring = "SELECT pgd_titel, pgd_vorname, pgd_name, pgd_strasse, pgd_plz, pgd_wohnort, (EXTRACT(YEAR FROM CURRENT_DATE)-EXRACT(YEAR FROM pgd_geburtsdatum)) as alter FROM pgd WHERE pgd_geburtsdatum BETWEEN dateBegin AND dateEnd"
  result <- liftIO $ handleSqlError $ quickQuery conn querystring []
  queryToHtml result birthdayForm
