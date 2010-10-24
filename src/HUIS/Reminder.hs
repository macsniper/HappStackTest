module HUIS.Reminder where

import Text.XHtml.Transitional hiding (dir,content)
import Data.Time (formatTime, getCurrentTime, UTCTime)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad
import Happstack.Server
import Database.HDBC
import HUIS.Database
import HUIS.StaticResponses
import Database.HDBC.Sqlite3
import Data.List
import Data.Time.Clock
import Data.Time.Calendar


data Reminder = Reminder{userName :: String
                        ,erledigen :: String
                        ,erledigt :: String
                        ,art :: String
                        ,content :: String
                        ,datum :: String
                        ,userGet :: String
                        ,erledigtGet :: String}

instance FromData Reminder where
  fromData = do
  user <- lookRead "user"
  frist <- lookRead "erledigen"
  notiz <- look "inhalt"
  fertig <- lookRead "erledigt"
  notizart <- lookRead "art"
  datumAbf <- look "datum"
  userAbf <- look "userGet"
  erledigtAbf <- look "erledigtGet"
  return $ Reminder{userName = user, erledigen = frist, erledigt = fertig, content = notiz, art = notizart, datum = datumAbf, userGet = userAbf, erledigtGet = erledigtAbf}

nullReminder :: Reminder
nullReminder = Reminder{userName = ""
                        ,erledigen = ""
                        ,erledigt = ""
                        ,art = ""
                        ,content = ""
                        ,datum = ""
                        ,userGet=""
                        ,erledigtGet=""}


reminderForm:: [Html]
reminderForm =
  [ gui "/reminder" <<
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
    , select ! [name "art"] << [ option << p << "Telefonnotiz"
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
    , submit "run" "anlegen"]
    , gui "/reminderGet" <<
    [ label << "Wiedervorlage anzeigen"
    , br
    , textfield "datum" ! [size "30", value  "JJJJ-MM-TT"]
    , br
    , textfield "userGet" ! [size "30", value "Bitte Name eingeben"]
    , br
    , select ! [name "erledigtGet"] << [option << p << "Nein"
                                    ,option << p << "Ja"
                                    ]
    , br
    , submit "run" "anzeigen"]
  , thediv << "HUIS-Reminder"
  ]

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay


saveReminderToDb:: Connection -> Reminder -> ServerPart Response
saveReminderToDb conn req = do
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



