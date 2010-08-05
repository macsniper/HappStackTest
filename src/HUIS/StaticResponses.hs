module HUIS.StaticResponses where

import Text.XHtml.Transitional hiding (dir)
import Happstack.Server
import Paths_HUIS



-- | Shows the start (index) page.
showStartPage:: ServerPart Response
showStartPage =
  ok $ toResponse $ thehtml <<
    [ header << headerContent "HUIS-Startseite"
    , body << thediv ! [theclass "main"] << concat [(bodyHeader "/"), slider, bgforsearch, contentWrap startPageContent, footer]]


startPageContent:: [Html]
startPageContent = [noHtml]

-- | Serves a file, identified by 'filedir' (without trailing \/) and 'filename'.
showFile:: String-> String-> ServerPart Response
showFile filedir filename =
  serveFileUsing filePathLazy (guessContentTypeM mimeTypes) (filedir ++ "/" ++ filename)

-- | Main function for serving a static web page. Build the site from functions defined in this module ('headerContent', 'upperBody', 'lowerBody')
-- | and expects a title and a function returning the contents of the site.
showPage:: String-> String-> [Html]-> ServerPart Response
showPage title location content =
  ok $ toResponse $ thehtml <<
    [ header << headerContent title
    , body << thediv ! [theclass "main"] << concat [(bodyHeader location), slider, bgforsearch, contentWrap content, footer]]

showPageWithData:: String-> (a -> [Html])-> a -> ServerPart Response
showPageWithData title content reqdata =
  showPage title  "/"  (content reqdata)

-- | Function for the header-part of every page.
headerContent:: String-> [Html]
headerContent title =
  [ thetitle << stringToHtml title
  , script ! [thetype "text/javascript", src "/ressources/js/jquery.js"] << noHtml --workaround, as script expects an Html as param
  , script ! [thetype "text/javascript", src "/ressources/js/easySlider1.5.js"] << noHtml --workaround, as script expects an Html as param
  , script ! [thetype "text/javascript", src "/ressources/js/slider.js"] << noHtml --workaround, as script expects an Html as param
  , thelink ! [rel "stylesheet", thetype "text/css", href "/ressources/css/style.css"] << noHtml
  ]


bodyHeader:: String-> [Html]
bodyHeader active =
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
              ,li << anchor ! [href "/"] << [
                thespan << "Start"
              ]
              ,li << noHtml
              ,li << anchor ! [href "/"] << [
                thespan << "Start"
              ]
            ]
          ]
        ],
        thediv ! [theclass "clr"] << noHtml
      ]
    ]
  ]

slider:: [Html]
slider = [
  thediv ! [theclass "slider"] << [
    thediv ! [theclass "slider_resize"] << [
        thediv ! [identifier "slider"] << [
          ulist << [
            li << [
              thediv << [
                p ! [theclass "img"] << [
                  image ! [src "/ressources/img/simple_text_img_1.png", alt "screen 1", width "251", height "205"]
                ],
                h2 << "Verwalten ...",
                p << "ganz einfach mit dem neuen HUiS der Uni Duisburg - Essen !"
              ]
            ],li << [
              thediv << [
                p ! [theclass "img"] << [
                  image ! [src "/ressources/img/simple_text_img_1.png", alt "screen 1", width "251", height "205"]
                ],
                h2 << "Verwalten ...",
                p << "ganz einfach mit dem neuen HUiS der Uni Duisburg - Essen !"
              ]
            ],li << [
              thediv << [
                p ! [theclass "img"] << [
                  image ! [src "/ressources/img/simple_text_img_1.png", alt "screen 1", width "251", height "205"]
                ],
                h2 << "Verwalten ...",
                p << "ganz einfach mit dem neuen HUiS der Uni Duisburg - Essen !"
              ]
            ]
          ]
        ]
      ]
    ]
  ]

slider2:: String-> String-> [Html]
slider2 headl cont = [
    thediv ! [theclass "slider2"] << [
      thediv ! [theclass "slider2_resize"] << [
        h2 << headl,
        p << cont
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
