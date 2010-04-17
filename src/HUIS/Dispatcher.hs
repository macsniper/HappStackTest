module HUIS.Dispatcher where

import Happstack.Server
import Control.Monad (msum)
import qualified Data.Map
import HUIS.StaticResponses

runDispatcher:: ServerPart Response
runDispatcher = msum 
  -- GET '/' => show start page | simple authentification
  [ basicAuth "HUIS" (Data.Map.fromList [("admin", "password")]) (methodOnly GET  >> nullDir >> showStartPage)
  -- GET '/ressources/img/*' => serve image-file
  , methodOnly GET >> dir "ressources" ( dir "img" ( path (showFile "img")))
  -- GET '/ressources/css/*' => serve css-file
  , methodOnly GET >> dir "ressources" ( dir "css" ( path (showFile "css")))
  -- GET '/ressources/js/*' => serve javascript-file
  , methodOnly GET >> dir "ressources" ( dir "js" ( path (showFile "js")))
  
  -- add other requests here
  -- eol
  ]
--TODO: divide dispatcher in internal and non-internal methods (for login etc).