-- |This module contains the function to dispatch requests.
module HUIS.Dispatcher(runDispatcher) where

import Happstack.Server
import Control.Monad (msum)
import HUIS.StaticResponses
import HUIS.SimpleQuery
import qualified Network.Gitit
import HUIS.ConfigParser
import Data.Map hiding(map)
import Database.HDBC.ODBC
import HUIS.Anniversary
import HUIS.Reminder
import HUIS.Birthday

staticdirs:: [String]
staticdirs = ["js", "css", "img"]

-- |Main function for dispatching requests. Currently handles the following:
--
-- \- files in 'staticdirs' (js, css, img)
--
-- \- start page (GET /)
--
-- \- gitit-wiki (GET /wiki/)
runDispatcher:: Config-> Network.Gitit.Config-> Connection-> ServerPart Response
runDispatcher conf wikiconf connection = msum
  -- GET '/' => show start page
  [ methodM GET >> nullDir >> showStartPage
  -- GET '/ressources/css|js|img)/*'
  , msum $ map (serveStaticFile (conf ! "ressourcedir")) staticdirs
  -- GET '/wiki'
  , dir "wiki" $ Network.Gitit.wiki wikiconf
  -- buggy Wiki-login redirect - fix for
  , dir "_login" $ seeOther (conf ! "wikidir" ++ "_login") $ toResponse()
  -- simple QUERY interface
  , dir "simplequery" $ methodSP POST $ withData (simpleQueryResult connection)
  , dir "simplequery" $ methodSP GET $ showPage "Einfaches Query-Interface" "/" simpleQueryForm
  -- anniversary-query stuff
  , dir "anniversary" $ methodSP POST $ withData (anniversaryResult connection)
  , dir "anniversary" $ methodSP GET $ showPage "Anniversary Query" "/" anniversaryForm
  -- birthday-query stuff
  , dir "birthday" $ methodSP POST $ withData (birthdayResult connection)
  , dir "birthday" $ methodSP GET $ showPage "Birthday Query" "/" birthdayForm
  -- reminder
  --, dir "reminder" $ methodSP POST $ withData (reminderResult connection)
  , dir "reminder" $ methodSP GET $ showPage "Reminder Query" "/" reminderForm
  ]

serveStaticFile:: String-> String-> ServerPart Response
serveStaticFile rdir sdir = methodOnly GET >> dir "ressources" ( dir sdir ( path $ showFile (rdir ++ sdir)))
