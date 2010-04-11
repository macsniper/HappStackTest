module HUIS.Dispatcher where

import Happstack.Server
import Control.Monad (msum)

import HUIS.StaticResponses

runDispatcher:: ServerPart Response
runDispatcher = msum 
  -- GET '/' => show start page
  [ methodOnly GET  >> nullDir >> showStartPage
  -- GET '/ressources/img/*' => serve image-file
  , methodOnly GET >> dir "ressources" ( dir "img" ( path (showImageFile)))
  -- add other requests here
  -- eol
  ]
