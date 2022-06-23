module CustomColor exposing (..)

import Color


colorPalette =
    { primary = Color.rgb255 41 98 255
    , secondary = Color.rgb255 230 74 25
    , background = Color.rgb255 255 255 255
    , surface = Color.rgb255 255 255 255
    , error = Color.rgb255 176 0 32
    , on =
        { primary = Color.rgb255 255 255 255
        , secondary = Color.rgb255 0 0 0
        , background = Color.rgb255 0 0 0
        , surface = Color.rgb255 0 0 0
        , error = Color.rgb255 176 0 32
        }
    }


text =
    Color.rgb255 0 0 0
