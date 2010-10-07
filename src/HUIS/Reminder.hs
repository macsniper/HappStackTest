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
                    " VALUES(?, ?, ?, ?, ?, ?)"
        t <- liftIO getCurrentTime
        let vals = [toSql (userName reminder), toSql t, toSql t, toSql t, toSql (erledigt reminder),
                    toSql (art reminder), toSql (content reminder)]
        liftIO $ withTransaction db $ \d -> run d query vals
        [[uid]] <- liftIO $ quickQuery db "select last_insert_rowid()" []
        return (fromSql uid)

{-getReminderFromDb :: (IConnection d, MonadIO m, MonadPlus m)
                    => d -> Integer -> m Reminder
getReminderFromDb db uid = do
        reminders <- liftIO $ handleSqlError $
                    quickQuery db "SELECT * FROM pbv_wiedervl WHERE id = ?" [toSql uid]
        case reminders of
           ([_,tit,ts,synt,cont,jn,dw,wd]:_) ->
                    return Reminder { userID = uid
                                    , erstellt = fromSql ts
                                    , bearbeitet = fromSql synt
                                    , erledigen = fromSql cont
                                    , erledigt = fromSql jn
                                    , art = fromSql dw
                                    , content = fromSql wd}
           _ -> mzero-}

reminderResult:: Connection -> Reminder -> ServerPart Response
reminderResult conn req = do
  let querystring = "SELECT * FROM pbv_wiedervl WHERE zu_erledigen_bis =" ++ erledigen req ++ "AND erstellt_von" ++ userName req ++ "AND erledigt" ++ erledigt req ++ "AND art" ++ art req ++ ";"
  result <- liftIO $ handleSqlError $ quickQuery conn querystring []
  queryToHtml [] result reminderForm


