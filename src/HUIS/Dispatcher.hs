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
  [ methodM GET >> nullDir >> showPage "HUIS-Startseite" showStartPage
  -- GET '/ressources/css|js|img)/*'
  , msum $ map (serveStaticFile (conf ! "ressourcedir")) staticdirs
  -- GET '/wiki'
  , dir "wiki" $ Network.Gitit.wiki wikiconf
  -- buggy Wiki-login redirect - fix for
  , dir "_login" $ seeOther (conf ! "wikidir" ++ "_login") $ toResponse()
  -- simple QUERY interface
  , dir "simplequery" $ methodSP POST $ withData (showPageWithData "Einfaches Query-Interface" (simpleQueryResult connection))
  , dir "simplequery" $ methodSP GET $ showPage "Einfaches Query-Interface" simpleQueryForm
  -- add other requests here
  ]

serveStaticFile:: String-> String-> ServerPart Response
serveStaticFile rdir sdir = methodOnly GET >> dir "ressources" ( dir sdir ( path $ showFile (rdir ++ sdir)))