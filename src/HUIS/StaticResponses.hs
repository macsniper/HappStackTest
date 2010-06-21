module HUIS.StaticResponses where

import Text.XHtml.Transitional hiding (dir)
import Happstack.Server
import Paths_HUIS



-- | Shows the start (index) page.
showStartPage:: [Html]
showStartPage =
  [ thediv << "Herzlich Willkommen!"
  , anchor ! [href "/wiki"] << "Zum Wiki"]

-- | Serves a file, identified by 'filedir' (without trailing \/) and 'filename'. 
showFile:: String-> String-> ServerPart Response
showFile filedir filename = 
  serveFileUsing filePathLazy (guessContentTypeM mimeTypes) (filedir ++ "/" ++ filename)

-- | Main function for serving a static web page. Build the site from functions defined in this module ('headerContent', 'upperBody', 'lowerBody')
-- | and expects a title and a function returning the contents of the site.
showPage:: String-> [Html]-> ServerPart Response
showPage title content = 
  ok $ toResponse $ thehtml <<
    [ header << headerContent title
    , body << concat [upperBody, content, lowerBody]]  
    
showPageWithData:: String-> (a ->[Html])-> a -> ServerPart Response
showPageWithData title content reqdata = 
  showPage title (content reqdata)    
    
-- | Function for the header-part of every page.
headerContent:: String-> [Html]
headerContent title = 
  [ thetitle << stringToHtml title
  , script ! [thetype "text/javascript", src "/ressources/js/prototype.js"] << noHtml --workaround, as script expects an Html as param
  , thelink ! [rel "stylesheet", thetype "text/css", href "/ressources/css/default.css"] << noHtml
  ]

-- | Function for the upper-body-part (e.g. menu) of every page.
upperBody:: [Html]
upperBody = 
  [ image ! [src "/ressources/img/logo.png"]
  ]

-- | Function for the footer-part of every page.
lowerBody:: [Html]
lowerBody = 
  [ br, br, br, br, br
  , hr
  , thediv ! [theclass "footer"] << "HUIS, created with Happstack"]                     