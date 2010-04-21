module HUIS.Dispatcher(runDispatcher, wikiAuthDispatcher) where

import Happstack.Server
import Control.Monad (msum)
import qualified Data.Map
import HUIS.StaticResponses
import qualified Network.Gitit
import HUIS.ConfigParser

runDispatcher:: Config-> Network.Gitit.Config-> ServerPart Response
runDispatcher conf wikiconf = msum 
  -- GET '/' => show start page | simple authentification
  [ basicAuth "HUIS" (Data.Map.fromList [("admin", "password")]) (methodOnly GET  >> nullDir >> showStartPage)
  -- GET '/ressources/img/*' => serve image-file
  , methodOnly GET >> dir "ressources" ( dir "img" ( path (showFile "img")))
  -- GET '/ressources/css/*' => serve css-file
  , methodOnly GET >> dir "ressources" ( dir "css" ( path (showFile "css")))
  -- GET '/ressources/js/*' => serve javascript-file
  , methodOnly GET >> dir "ressources" ( dir "js" ( path (showFile "js")))
  -- GET '/wiki/' => serve wiki
  , dir "wiki" $ Network.Gitit.wiki wikiconf
  
  -- add other requests here
  -- eol
  ]
--TODO: divide dispatcher in internal and non-internal methods (for login etc).

-- needed for gitit-wiki to redirect logins
wikiAuthDispatcher wikidir = msum
   [ dir "_login"  $ seeOther (wikidir ++ "/_login")  $ toResponse ()
   , dir "_logout" $ seeOther (wikidir ++ "/_logout") $ toResponse () ]
