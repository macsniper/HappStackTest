module HUIS.Reminder where

import Text.XHtml.Transitional hiding (dir,content)
import Data.Time (formatTime, getCurrentTime, UTCTime)
import Control.Monad.Trans (MonadIO, liftIO)
import Happstack.Server

data Reminder = Reminder{userID :: Int
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
  return $ Reminder{userID = user, erledigen = frist, content = notiz, art = notizart}

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
    , submit "run" "starten" ]
  , thediv << "HUIS-Reminder"
  ]
  
  
  
  