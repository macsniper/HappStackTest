module HUIS.Anniversary where

import Text.XHtml.Transitional hiding (dir,content)
import Happstack.Server
import HUIS.Database
import HUIS.StaticResponses
import Database.HDBC
import Database.HDBC.ODBC
import Control.Monad.Trans (MonadIO, liftIO)


data DateRange = DateRange{from :: String,
                           to :: String,
                           number :: String}

instance FromData DateRange where
  fromData = do
  dateBeginn <- look "date1"
  dateEnd <- look "date2"
  year <- look "years" --TODO: Check for correct Value
  return $ DateRange{from = dateBeginn, to = dateEnd, number = year}


anniversaryForm:: [Html]
anniversaryForm =
  [ gui "/anniversary" <<
    [ label << "Date 1"
    , textfield "date1" ! [size "30", value "'yyyy-mm-dd'"]
    , label << "Date2"
    , textfield "date2" ! [size "30", value "'yyyy-mm-dd'"]
    , label << "Number of years"
    , textfield "years" ! [size "30", value "1"]
    , submit "run" "starten" ]
  , thediv << "HUIS-Anniversary"
  ]

anniversaryResult:: Connection-> DateRange-> ServerPart Response
anniversaryResult conn temp = do
  let querystring = "SELECT pgd_geschlecht, pgd_name, pgd_namenbestand, pgd_titel, pgd_vornamen, pgd_strasse, pgd_plz, pgd_wohnort, pgd_jubi_berech, pgd_dienstzeit, pgd_beschaeft_von, druck_anredetitelm, lname1 FROM ((pgd left outer join k_anredetitel on (pgd.pgd_titel = k_anredetitel.key_anredetitel)) left outer join pfi on (pgd.pgd_join_id = pfi.pfi_pgd_join_id AND pfi.pfi_bis > CURRENT)) left outer join inst on (pfi.poz_institut = inst.inst_nr) WHERE (pgd_austrittsdatum > CURRENT) AND (((pgd_jubi_berech is not null) AND ((pgd_jubi_berech + " ++ number temp ++ " units YEAR) BETWEEN '" ++ from temp ++ "' AND '" ++ to temp ++ "')) OR ((pgd_jubi_berech is null AND pgd_dienstzeit is not null) AND ((pgd_dienstzeit + " ++ number temp ++ " units YEAR) BETWEEN '" ++ from temp ++ "' AND '" ++ to temp ++ "')) OR ((pgd_jubi_berech is null AND pgd_dienstzeit is null AND pgd_beschaeft_von is not null) AND ((pgd_beschaeft_von + " ++ number temp ++ " units YEAR) BETWEEN '" ++ from temp ++ "' AND '" ++ to temp ++ "')));"
  result <- liftIO $ handleSqlError $ quickQuery conn querystring []
  queryToHtml result anniversaryForm

