module HUIS.Dispatcher(runDispatcher) where

import Happstack.Server
import Control.Monad (msum)
import HUIS.StaticResponses
import qualified Network.Gitit
import HUIS.ConfigParser
import Data.Map hiding(map)

staticdirs:: [String]
staticdirs = ["js", "css", "img"]

runDispatcher:: Config-> Network.Gitit.Config-> ServerPart Response
runDispatcher conf wikiconf = msum 
  -- GET '/' => show start page | simple authentification
  [ basicAuth "HUIS" (fromList [("admin", "password")]) (methodOnly GET  >> nullDir >> showStartPage (conf ! "ressourcedir"))
  -- GET '/ressources/css|js|img)/*'
  , msum $ map (serveStaticFile (conf ! "ressourcedir")) staticdirs
  -- GET '/wiki' 
  , dir "wiki" $ Network.Gitit.wiki wikiconf
  -- buggy Wiki-login redirect - fix for
  , dir "_login" $ seeOther (conf ! "wikidir" ++ "_login") $ toResponse()
  
  -- add other requests here
  -- eol
  ]
--TODO: divide dispatcher in internal and non-internal methods (for login etc).

serveStaticFile:: String-> String-> ServerPart Response
serveStaticFile rdir sdir = methodOnly GET >> dir "ressources" ( dir sdir ( path $ showFile (rdir ++ sdir)))