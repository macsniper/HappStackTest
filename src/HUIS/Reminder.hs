module HUIS.Reminder where

import Text.XHtml.Transitional hiding (dir,content)
import Data.Time (formatTime, getCurrentTime, UTCTime)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad
import Happstack.Server
import Database.HDBC

data Reminder = Reminder{userID :: Integer
                        ,erstellt :: UTCTime
                        ,bearbeitet :: UTCTime
                        ,erledigen :: UTCTime
                        ,erledigt :: Bool
                        ,art :: Int
                        ,content :: String}

instance FromData Reminder where
  fromData = do
  user <- lookRead "user"
  frist <- lookRead "erledigen"
  notiz <- look "inhalt"
  notizart <- lookRead "art"
  return $ nullReminder{userID = user, erledigen = frist, content = notiz, art = notizart}

nullReminder :: Reminder
nullReminder = Reminder{userID = undefined
                        ,erstellt = undefined
                        ,bearbeitet = undefined
                        ,erledigen = undefined
                        ,erledigt = False
                        ,art = undefined
                        ,content = "" }


reminderForm:: [Html]
reminderForm =
  [ gui "/Reminder" <<
    [ label << "User"
    , textfield "user" ! [size "30", value ""]
    , label << "zu erledigen bis"
    , textfield "erledigen" ! [size "30", value ""]
    , label << "Inhalt"
    , textfield "inhalt" ! [size "30", value ""]
    , label << "Art der Notiz"
    , textfield "art" ! [size "30", value ""]
    , submit "run" "anlegen" ]
  , thediv << "HUIS-Reminder"
  ]

saveReminderToDb:: (IConnection d, MonadIO m) => d -> Reminder -> m Integer
saveReminderToDb db reminder = do
        let query = "INSERT INTO pbv_wiedervl(erstellt_von, erstellt, zuletzt_bearbeitet, zu_erledigen_bis, erledigt, art, kunde)" ++
                    " VALUES(?, ?, ?, ?, ?, ?, ?)"
        t <- liftIO getCurrentTime
        let vals = [toSql (userID reminder), toSql t, toSql t, toSql t, toSql (erledigt reminder),
                    toSql (art reminder), toSql (content reminder)]
        liftIO $ withTransaction db $ \d -> run d query vals
        [[uid]] <- liftIO $ quickQuery db "select last_insert_rowid()" []
        return (fromSql uid)

getReminderFromDb :: (IConnection d, MonadIO m, MonadPlus m)
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
           _ -> mzero

