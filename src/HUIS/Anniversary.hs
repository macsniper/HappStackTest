module HUIS.Anniversary where

import Text.XHtml.Transitional hiding (dir,content)
import Happstack.Server
import HUIS.Database
import HUIS.StaticResponses
import Database.HDBC
import Database.HDBC.ODBC
import Control.Monad.Trans (MonadIO, liftIO)


data DateRangeN = DateRangeN{from :: String,
                           to :: String,
                           number :: String}

nullDateRangeN = DateRangeN{from = "yyyy-mm-dd", to = "yyyy-mm-dd", number = "25"}

instance FromData DateRangeN where
  fromData = do
  dateBeginn <- look "date1"
  dateEnd <- look "date2"
  year <- look "years" --TODO: Check for correct Value
  return $ DateRangeN{from = dateBeginn, to = dateEnd, number = year}


anniversaryForm:: DateRangeN-> [Html]
anniversaryForm formdata =
  [ gui "/anniversary" <<
        [
      table ! [width "100%"] << [
        tr << [
          td ! [colspan 2, align "center"] << h2 << "Übersicht über Jubiläen"
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
        ], tr << [
          td ! [thestyle "width: 115px;"] << label << "Jubiläum",
          td ! [thestyle "width: 115px;"] << select ! [name "years"] <<
          map (createOptionForYears $ number formdata) ["25", "40", "50"]
        ],tr << [
          td << noHtml,
          td << submit "run" "Jubilare anzeigen"
        ]
      ]
    ]
  ]

createOptionForYears:: String-> String-> Html
createOptionForYears selected year =
  option ! [value year, (isSelected year selected)] << (year ++ " Jahre")

isSelected:: String-> String-> HtmlAttr
isSelected should isreal =
  if should == isreal
  then selected
  else thestyle "" --  ugly workaround, as no "noHtmlAttr" exists

anniversaryResult:: Connection-> DateRangeN-> ServerPart Response
anniversaryResult conn temp = do
  let querystring = "SELECT pgd_geschlecht, pgd_name, pgd_namenbestand, pgd_titel, pgd_vornamen, pgd_strasse, pgd_plz, pgd_wohnort, pgd_jubi_berech, pgd_dienstzeit, pgd_beschaeft_von, druck_anredetitelm, lname1 FROM ((pgd left outer join k_anredetitel on (pgd.pgd_titel = k_anredetitel.key_anredetitel)) left outer join pfi on (pgd.pgd_join_id = pfi.pfi_pgd_join_id AND pfi.pfi_bis > CURRENT)) left outer join inst on (pfi.poz_institut = inst.inst_nr) WHERE (pgd_austrittsdatum > CURRENT) AND (((pgd_jubi_berech is not null) AND ((pgd_jubi_berech + " ++ number temp ++ " units YEAR) BETWEEN '" ++ from temp ++ "' AND '" ++ to temp ++ "')) OR ((pgd_jubi_berech is null AND pgd_dienstzeit is not null) AND ((pgd_dienstzeit + " ++ number temp ++ " units YEAR) BETWEEN '" ++ from temp ++ "' AND '" ++ to temp ++ "')) OR ((pgd_jubi_berech is null AND pgd_dienstzeit is null AND pgd_beschaeft_von is not null) AND ((pgd_beschaeft_von + " ++ number temp ++ " units YEAR) BETWEEN '" ++ from temp ++ "' AND '" ++ to temp ++ "')));"
  result <- liftIO $ handleSqlError $ quickQuery conn querystring []
  let result' = map convertResult result
  queryToHtml columns result' (anniversaryForm temp)

columns = ["Anrede",
           "Titel",
           "Vorname(n)", "Zusatz", "Nachname", "Straße", "PLZ", "Ort",
           "Jubiläumsdatum", "Institut"]


convertResult:: [SqlValue]-> [SqlValue]
convertResult [gender,name,namebestand,leeresfeld,vornamen,strasse,plz,ort,datum1,datum2,datum3,titel,institut] =
  [(selectGender gender),titel,vornamen,namebestand,name,strasse,plz,ort,(selectDate datum1 datum2 datum3),institut]

selectGender:: SqlValue-> SqlValue
selectGender gen =
  if fromSql gen == "M"
  then toSql "Herr"
  else toSql "Frau"

selectDate:: SqlValue-> SqlValue -> SqlValue -> SqlValue
selectDate datum1 datum2 datum3 =
  if fromSql datum1 /= ""
  then datum1
  else if fromSql datum2 /= ""
       then datum2
       else datum3
