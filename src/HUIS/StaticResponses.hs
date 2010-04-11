module HUIS.StaticResponses where

import Text.XHtml.Transitional hiding (dir)
import Happstack.Server

{-
every function inside here should be named like "showPageName"
serves the following static pages:

* start page
* static files in /ressources
-}

showStartPage:: ServerPart Response
showStartPage = 
  ok $ toResponse $ body << 
    [ image ! [src "/ressources/img/logo.png"]
    , br
    , thediv << "herzlich willkommen!"]

showImageFile:: String-> ServerPart Response
showImageFile filedir = 
  serveFileUsing filePathLazy (guessContentTypeM mimeTypes) ("./ressources/img/" ++ filedir)