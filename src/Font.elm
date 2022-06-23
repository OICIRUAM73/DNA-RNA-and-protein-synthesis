module Font exposing (..)

import Color exposing (Color)
import Element
import Element.Font


headline : Color -> List (Element.Attribute msg)
headline color =
    regular color 28 0.0


body : Color -> List (Element.Attribute msg)
body color =
    regular color 16 0.25


regular : Color -> Int -> Float -> List (Element.Attribute msg)
regular color size letterSpacing =
    [ fontAttr "Alegreya Sans" Element.Font.sansSerif
    , Element.Font.color <| Element.fromRgb <| Color.toRgba color
    , Element.Font.letterSpacing letterSpacing
    , Element.Font.size size
    ]


fontAttr : String -> Element.Font.Font -> Element.Attribute msg
fontAttr typeface fallback =
    Element.Font.family
        [ Element.Font.typeface typeface
        , fallback
        ]
