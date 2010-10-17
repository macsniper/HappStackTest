module HUIS.Reminder where

import Text.XHtml.Transitional hiding (dir,content)
import Data.Time (formatTime, getCurrentTime, UTCTime)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad
import Happstack.Server
import Database.HDBC
import HUIS.Database
import HUIS.StaticResponses
import Database.HDBC.PostgreSQL
import Data.List
import Data.Time.Clock
import Data.Time.Calendar


data Reminder = Reminder{userName :: String
                        ,erledigen :: String
                        ,erledigt :: String
                        ,art :: String
                        ,content :: String}

instance FromData Reminder where
  fromData = do
  user <- lookRead "user"
  frist <- lookRead "erledigen"
  notiz <- look "inhalt"
  fertig <- lookRead "erledigt"
  notizart <- lookRead "art"
  return $ Reminder{userName = user, erledigen = frist, erledigt = fertig, content = notiz, art = notizart}

nullReminder :: Reminder
nullReminder = Reminder{userName = undefined
                        ,erledigen = undefined
                        ,erledigt = undefined
                        ,art = undefined
                        ,content = "" }


reminderForm:: [Html]
reminderForm =
  [ gui "/Reminder" <<
    [ label << "User"
    , textfield "user" ! [size "30", value ""]
    , br
    , label << "zu erledigen bis"
    , textfield "erledigen" ! [size "30", value ""]
    , br
    , label << "Inhalt"
    , br
    , textarea  ! [ name "inhalt"
                  , rows "10"
                  , cols "40" ] << [ "Text eingeben" ]
    , br
    , label << "Art der Notiz"
    , select ! [name "art"] << [ option << p << ""
                               , option << p << "Telefonnotiz"
                               , option << p << "Gesprächsnotiz"
                               , option << p << "Geburtsurkunde"
                               , option << p << "Elternzeitantrag"
                               , option << p << "sonstige Hinweise"
                               ]
    , br
    , label << "Erledigt?"
    , select ! [name "erledigt"] << [option << p << "Nein"
                                    ,option << p << "Ja"
                                    ]
    , br
    , submit "run" "anlegen"
    , br
    , label << "Wiedervorlage vom "
    , textfield "datum" ! [size "30", value  "JJJJ-MM-TT"]
    , label << "anzeigen."
    , br
    , submit "run2" "aufrufen"]

  , thediv << "HUIS-Reminder"
  ]

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay


saveReminderToDb:: (IConnection d, MonadIO m) => d -> Reminder -> m Integer
saveReminderToDb db reminder = do
        let query = "INSERT INTO pbv_wiedervl(erstellt_von, erstellt, zuletzt_bearbeitet, zu_erledigen_bis, erledigt, art, kunde)" ++
                     " VALUES(" ++ userName req ++ ", current_date,current_date," ++ erledigen req ++ "," ++ erledigt req ++ "," ++ art req ++ "," ++ content req ++ ")"
         liftIO $ handleSqlError $ run conn query []
        dir "reminder" $ methodSP GET $ showPage "Reminder Query" reminderForm

makeWhereQuery :: Reminder -> String
makeWhereQuery input =
    if datum input == "" then " "
        else "AND zu_erledigen_bis =" ++ datum input

makeWhereQueryUser :: Reminder -> String
makeWhereQueryUser input =
    if userGet input == "" then " "
        else "AND user =" ++ userGet input

makeWhereQueryErl :: Reminder -> String
makeWhereQueryErl input =
    if erledigtGet input == "" then " "
        else "AND erledigt =" ++ erledigtGet input

reminderResult:: Connection -> Reminder -> ServerPart Response
reminderResult conn req = do
  let querystring = "SELECT * FROM pbv_wiedervl WHERE TRUE" ++ makeWhereQuery req ++ userGet req ++ erledigtGet req ++ ";"
  result <- liftIO $ handleSqlError $ quickQuery conn querystring []
  queryToHtml [] result reminderForm


