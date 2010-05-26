module HUIS.StaticResponses where

import Text.XHtml.Transitional hiding (dir)
import Happstack.Server
import Paths_HUIS

{-
every function inside here should be named like "showPageName"
serves the following static pages:

* start page
* static image files in /ressources/img
-}


showStartPage:: String-> ServerPart Response
showStartPage filedir =
  fileServe ["login.html"] (filedir ++ "html/")
  {-ok $ toResponse $ thehtml <<
    [ header <<
        [ thetitle << stringToHtml "HUIS"
        , script ! [thetype "text/javascript", src "/ressources/js/prototype.js"] << noHtml --workaround, as script expects an Html as param
        , style ! [thetype "text/css", src "/ressources/css/default.css"] << noHtml
      ]
    , body <<
        [ image ! [src "/ressources/img/logo.png"]
        , thediv << "Herzlich Willkommen!"
      ]
    ]
-}

showFile:: String-> String-> ServerPart Response
showFile filedir filename = --TODO: replace static path with one from config-file
  serveFileUsing filePathLazy (guessContentTypeM mimeTypes) (filedir ++ "/" ++ filename)
