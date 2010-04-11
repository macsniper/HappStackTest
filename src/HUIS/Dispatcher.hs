module HUIS.Dispatcher where

import Happstack.Server
import Control.Monad (msum)

import HUIS.StaticResponses

runDispatcher:: ServerPart Response
runDispatcher = msum 
  -- GET '/' => show start page
  [ methodOnly GET  >> nullDir >> showStartPage
  -- GET '/ressources/img/*' => serve image-file
  , methodOnly GET >> dir "ressources" ( dir "img" ( path (showFile "img")))
  -- GET '/ressources/css/*' => serve css-file
  , methodOnly GET >> dir "ressources" ( dir "css" ( path (showFile "css")))
  -- GET '/ressources/js/*' => serve javascript-file
  , methodOnly GET >> dir "ressources" ( dir "js" ( path (showFile "js")))
  
  -- add other requests here
  -- eol
  ]
