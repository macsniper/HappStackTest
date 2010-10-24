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

nullDateRange = DateRange{from = "JJJJ-MM-TT", to = "JJJJ-MM-TT"}

birthdayForm:: DateRange-> [Html]
birthdayForm formdata =
  [gui "/birthday" <<
        [
      table ! [width "100%"] << [
        tr << [
          td ! [colspan 2, align "center"] << h2 << "Geburtstagsliste"
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
          td << submit "run" "Geburtstage anzeigen",
          td << "input thetype\"button\"value\"CSV Speichern umschalten\"onclick\"this.form.gui=/CsvOutput\""
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
      querystring = "SELECT druck_anredetitelm, pgd_vornamen,pgd_namenbestand, pgd_name, lname1"
                    ++ ", (YEAR(CURRENT)-YEAR(pgd_geburtsdatum)) as alter, pgd_geburtsdatum"
                    ++ " FROM ((pgd left outer join k_anredetitel on (pgd.pgd_titel = k_anredetitel.key_anredetitel)) left outer join pfi on (pgd.pgd_join_id = pfi.pfi_pgd_join_id AND pfi.pfi_bis > CURRENT)) left outer join inst on (pfi.poz_institut = inst.inst_nr)"
                    ++ " WHERE (MONTH(pgd_geburtsdatum) > " ++ gebMonMin ++ " AND MONTH(pgd_geburtsdatum) < " ++ gebMonMax ++ ")"
                    ++ " OR (MONTH(pgd_geburtsdatum) = " ++ gebMonMin ++ " AND DAY(pgd_geburtsdatum) >= " ++ gebDayMin ++ ")"
                    ++ " OR (MONTH(pgd_geburtsdatum) = " ++ gebMonMax ++ " AND DAY(pgd_geburtsdatum) <= " ++ gebDayMax ++ ");"
  result <- liftIO $ handleSqlError $ quickQuery conn querystring []
  queryToHtml ["Titel", "Vorname(n)", "Zus.", "Nachname", "Institut", "Alter", "Geburtsdatum"] result $ birthdayForm dateRange


  birthdayResultOhneHtml:: Connection-> DateRange-> ServerPart Response
birthdayResultOhneHtml conn dateRange = do
  let (gebMonMin,gebDayMin) = mmDdOfDate $ from dateRange
      (gebMonMax,gebDayMax) = mmDdOfDate $ to dateRange
      querystring = "SELECT druck_anredetitelm, pgd_vornamen,pgd_namenbestand, pgd_name, lname1"
                    ++ ", (YEAR(CURRENT)-YEAR(pgd_geburtsdatum)) as alter, pgd_geburtsdatum"
                    ++ " FROM ((pgd left outer join k_anredetitel on (pgd.pgd_titel = k_anredetitel.key_anredetitel)) left outer join pfi on (pgd.pgd_join_id = pfi.pfi_pgd_join_id AND pfi.pfi_bis > CURRENT)) left outer join inst on (pfi.poz_institut = inst.inst_nr)"
                    ++ " WHERE (MONTH(pgd_geburtsdatum) > " ++ gebMonMin ++ " AND MONTH(pgd_geburtsdatum) < " ++ gebMonMax ++ ")"
                    ++ " OR (MONTH(pgd_geburtsdatum) = " ++ gebMonMin ++ " AND DAY(pgd_geburtsdatum) >= " ++ gebDayMin ++ ")"
                    ++ " OR (MONTH(pgd_geburtsdatum) = " ++ gebMonMax ++ " AND DAY(pgd_geburtsdatum) <= " ++ gebDayMax ++ ");"
  result <- liftIO $ handleSqlError $ quickQuery conn querystring []
