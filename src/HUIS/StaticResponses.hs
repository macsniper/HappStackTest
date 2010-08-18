module HUIS.StaticResponses where

import Text.XHtml.Transitional hiding (dir)
import Happstack.Server
import Database.HDBC
import Database.HDBC.ODBC
import HUIS.Database
import Paths_HUIS



-- | Shows the start (index) page.
showStartPage:: ServerPart Response
showStartPage =
  ok $ toResponse $ thehtml <<
    [ header << headerContent "HUIS-Startseite"
    , body << thediv ! [theclass "main"] << concat [(bodyHeader), slider2 "Startseite", bgforsearch, contentWrap startPageContent, footer]]


startPageContent:: [Html]
startPageContent =
  [
    table ! [width "100%"] << [
      tr << [
        td ! [colspan 6, align "center"] << h2 << "Willkommen bei HUIS"
      ],
      tr << [
        td ! [colspan 6, align "center"] << thediv ! [theclass "bg"] << noHtml
      ],
      tr << [
        td ! [thestyle "width: 115px;"] << image ! [src "/ressources/img/port_1.jpg"],
        td ! [valign "center", thestyle "width: 115px;"] << anchor ! [href "/birthday"] << "Geburtstagsliste",
        td ! [thestyle "width: 115px;"] << image ! [src "/ressources/img/port_1.jpg"],
        td ! [valign "center", thestyle "width: 115px;"] << anchor ! [href "/anniversary"] << "Jubiläen",
        td ! [thestyle "width: 115px;"] << image ! [src "/ressources/img/port_1.jpg"],
        td ! [valign "center", thestyle "width: 115px;"] << anchor ! [href "/simplequery"] << "Datenbankabfrage"
      ]
    ]
  ]

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
    , body << thediv ! [theclass "main"] << concat [bodyHeader, slider2 title, bgforsearch, contentWrap content, footer]]


showPageWithData:: String-> (a -> [Html])-> a -> ServerPart Response
showPageWithData title content reqdata =
  showPage title (content reqdata)


helpContent:: [Html]
helpContent =  [
    table ! [width "100%"] << [
      tr << [
        td ! [colspan 2, align "center"] << h2 << "HUIS-Hilfe"
      ],
      tr << [
        td ! [colspan 2, align "center"] << thediv ! [theclass "bg"] << noHtml
      ],
      tr << [
        td ! [colspan 2] << h3 << "Übersicht"
      ],
      tr << [
        td << noHtml,
        td << "folgt bald..."
      ]
    ]
  ]

-- | Function for the header-part of every page.
headerContent:: String-> [Html]
headerContent title =
  [ thetitle << stringToHtml title
  , script ! [thetype "text/javascript", src "/ressources/js/jquery.js"] << noHtml --workaround, as script expects an Html as param
  , thelink ! [rel "stylesheet", thetype "text/css", href "/ressources/css/style.css"] << noHtml
  ]


bodyHeader:: [Html]
bodyHeader =
  [thediv ! [theclass "header"] << [
      thediv ! [theclass "block_header"] << [
        thediv ! [theclass "logo"] << [
          anchor ! [href "/"] << [
            image ! [src "/ressources/img/logo.png", width "508", height "142", border 0, alt "logo"]
          ]
        ],
        thediv ! [thestyle "float:right"] << [
          thediv ! [theclass "menu"] << [
            ulist <<
              [li << anchor ! [href "/"] << [
                thespan << "Start"
              ]
              ,li << noHtml
              ,li << anchor ! [href "/wiki"] << [
                thespan << "Wiki"
              ]
              ,li << noHtml
              ,li << anchor ! [href "/help"] << [
                thespan << "Hilfe"
              ]
            ]
          ]
        ],
        thediv ! [theclass "clr"] << noHtml
      ]
    ]
  ]

slider2:: String-> [Html]
slider2 headl = [
    thediv ! [theclass "slider2"] << [
      thediv ! [theclass "slider2_resize"] << [
        h2 << headl,
        p << ""
      ]
    ]
  ]

bgforsearch:: [Html]
bgforsearch = [
    thediv ! [theclass "bg_for_search"] << [
      thediv ! [theclass "bg_for_search_resize"] << [
        thediv ! [theclass "search"] << [
          gui "/search" ! [identifier "form2", name "form2"] << [
            strong << stringToHtml "Suche:",
            input ! [thetype "text", name "eingabefeld", identifier "textfield", theclass "text"],
            input ! [thetype "image", name "imageField", identifier "imageField", src "/ressources/img/search.gif", theclass "button_search"]
          ]
        ]
      ]
    ]
  ]

footer:: [Html]
footer = [
    thediv ! [theclass "footer"] << [
      thediv ! [theclass "resize"] << [
        thediv << [
          br,
          anchor ! [href "#"] << "zurück zum Anfang"
        ]
      ], p ! [theclass "clr"] << noHtml
    ]
  ]

contentWrap:: [Html] -> [Html]
contentWrap content = [
    thediv ! [theclass "body"] << [
      thediv ! [theclass "body_resize"] << content
    ]
  ]


-- | Generating HTML-Output from Query
queryToHtml:: [String] -> [[SqlValue]]-> [Html]-> ServerPart Response
queryToHtml col stmt req =
  showPage "Ergebnisse der Anfrage" (req ++ resultTable col stmt)


resultTable:: [String] -> [[SqlValue]]-> [Html]
resultTable col res =
  [br, br, table ! [width "100%"] << tr << td ! [align "center"] << table ! [theclass "resulttable"] << [
    [tr << (map colValue col)],
    map resultLine res
  ]]

colValue:: String -> Html
colValue val = td ! [align "center"] << strong << val

resultLine:: [SqlValue]-> Html
resultLine resline =
  tr << map resultEntry resline

resultEntry:: SqlValue-> Html
resultEntry val = td << (stringToHtml $ newFromSql $ safeFromSql val)
