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
  dateBegin <- look "date1"
  dateEnd <- look "date2"
  return $ DateRange{from = dateBegin, to = dateEnd}

nullDateRange = DateRange{from = "yyyy-mm-dd", to = "yyyy-mm-dd"}

birthdayForm:: DateRange-> [Html]
birthdayForm formdata =
  [gui "/birthday" <<
        [
      table ! [width "100%"] << [
        tr << [
          td ! [colspan 2, align "center"] << h2 << "Geburtstagsübersicht"
        ],
        tr << [
          td ! [colspan 2, align "center"] << thediv ! [theclass "bg"] << noHtml
        ],
        tr << [
          td ! [thestyle "width: 115px;"] << label << "Startdatum",
          td ! [thestyle "width: 115px;"] << textfield "date1" ! [size "30", value $ from formdata]
        ],tr << [
          td ! [thestyle "width: 115px;"] << label << "Enddatum",
          td ! [thestyle "width: 115px;"] << textfield "date2" ! [size "30", value $ to formdata]
        ],tr << [
          td << noHtml,
          td << submit "run" "Geburtstage anzeigen"
        ]
      ]
    ]
  ]

mmDdOfDate:: String-> (String,String)
mmDdOfDate date =
    let mm'dd= (tail . snd .  break (=='-')) date
        (mm,_:dd)= break (=='-') mm'dd
    in (mm,dd)


birthdayResult:: Connection-> DateRange-> ServerPart Response
birthdayResult conn dateRange = do
  let (gebMonMin,gebDayMin) = mmDdOfDate $ from dateRange
      (gebMonMax,gebDayMax) = mmDdOfDate $ to dateRange
      querystring = "SELECT pgd_titel, pgd_vornamen, pgd_name, pgd_strasse, pgd_plz, pgd_wohnort"
                    ++ ", (YEAR(CURRENT)-YEAR(pgd_geburtsdatum)) as alter"
                    ++ " FROM pgd"
                    ++ " WHERE ((MONTH(pgd_geburtsdatum) BETWEEN (" ++ gebMonMin ++ " AND " ++ gebMonMax ++ ")"
                    ++ " AND (DAY(pgd_geburtsdatum) BETWEEN (" ++ gebDayMin ++ " AND " ++ gebDayMax ++ "))"
  result <- liftIO $ handleSqlError $ quickQuery conn querystring []
  queryToHtml [] result $ birthdayForm nullDateRange
