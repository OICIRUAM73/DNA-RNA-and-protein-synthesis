module Model exposing (..)

import Color exposing (Color)


type alias BaseSlice =
    { label : List String
    , value : List String
    , isActive : Bool
    , translate : Maybe BaseLabelTranslate
    , startAngle : Float
    , endAngle : Float
    , category : Maybe ProteinCategory
    }


type ProteinCategory
    = NonPolar
    | PolarUncharged
    | PositivelyCharged
    | NegativelyCharged
    | Special
    | Stop


type alias BaseLabelTranslate =
    { x : Float
    , y : Float
    }


colorPalette =
    { primary = Color.rgb255 26 35 126
    , secondary = Color.rgb255 61 90 254
    , background = Color.rgb255 255 255 255
    , surface = Color.rgb255 255 255 255
    , error = Color.rgb255 176 0 32
    , on =
        { primary = Color.rgb255 255 255 255
        , secondary = Color.rgb255 0 0 0
        , background = Color.rgb255 0 0 0
        , surface = Color.rgb255 0 0 0
        , error = Color.rgb255 255 255 255
        }
    }


getFirstRadialBaseSegment : List BaseSlice
getFirstRadialBaseSegment =
    [ { label = [ "C" ]
      , value = [ "C" ]
      , isActive = False
      , translate = Just { x = -2, y = 5 }
      , startAngle = 0
      , endAngle = 90
      , category = Nothing
      }
    , { label = [ "A" ]
      , value = [ "A" ]
      , translate = Just { x = 4, y = 5 }
      , isActive = False
      , startAngle = 90
      , endAngle = 180
      , category = Nothing
      }
    , { label = [ "G" ]
      , value = [ "G" ]
      , translate = Just { x = 4, y = 10 }
      , isActive = False
      , startAngle = 180
      , endAngle = 270
      , category = Nothing
      }
    , { label = [ "U" ]
      , value = [ "U" ]
      , translate = Just { x = -4, y = 10 }
      , isActive = False
      , startAngle = 270
      , endAngle = 360
      , category = Nothing
      }
    ]


getCytosineRadialSecondSegment : List BaseSlice
getCytosineRadialSecondSegment =
    let
        startAngle =
            0

        endAngle =
            90

        segmentAngle =
            (endAngle - startAngle) / 4
    in
    [ { label = [ "U" ]
      , value = [ "CU" ]
      , translate = Just { x = -20, y = 2 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 1
      , category = Nothing
      }
    , { label = [ "C" ]
      , value = [ "CC" ]
      , translate = Just { x = -20, y = -5 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Nothing
      }
    , { label = [ "A" ]
      , value = [ "CA" ]
      , translate = Just { x = -10, y = -10 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 3
      , category = Nothing
      }
    , { label = [ "G" ]
      , value = [ "CG" ]
      , translate = Just { x = -3, y = -15 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 4
      , category = Nothing
      }
    ]


getAdenineRadialSecondSegment : List BaseSlice
getAdenineRadialSecondSegment =
    let
        startAngle =
            90

        endAngle =
            180

        segmentAngle =
            (endAngle - startAngle) / 4
    in
    [ { label = [ "U" ]
      , value = [ "AU" ]
      , translate = Just { x = 5, y = -15 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 1
      , category = Nothing
      }
    , { label = [ "C" ]
      , value = [ "AC" ]
      , translate = Just { x = 10, y = -10 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Nothing
      }
    , { label = [ "A" ]
      , value = [ "AA" ]
      , translate = Just { x = 15, y = -5 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 3
      , category = Nothing
      }
    , { label = [ "G" ]
      , value = [ "AG" ]
      , translate = Just { x = 20, y = 2 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 4
      , category = Nothing
      }
    ]


getGuanineRadialSecondSegment : List BaseSlice
getGuanineRadialSecondSegment =
    let
        startAngle =
            180

        endAngle =
            270

        segmentAngle =
            (endAngle - startAngle) / 4
    in
    [ { label = [ "U" ]
      , value = [ "GU" ]
      , translate = Just { x = 20, y = 10 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 1
      , category = Nothing
      }
    , { label = [ "C" ]
      , value = [ "GC" ]
      , translate = Just { x = 15, y = 15 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Nothing
      }
    , { label = [ "A" ]
      , value = [ "GA" ]
      , translate = Just { x = 10, y = 20 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 3
      , category = Nothing
      }
    , { label = [ "G" ]
      , value = [ "GG" ]
      , translate = Just { x = 5, y = 25 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 4
      , category = Nothing
      }
    ]


getUracilRadialSecondSegment : List BaseSlice
getUracilRadialSecondSegment =
    let
        startAngle =
            270

        endAngle =
            360

        segmentAngle =
            (endAngle - startAngle) / 4
    in
    [ { label = [ "U" ]
      , value = [ "UU" ]
      , translate = Just { x = -5, y = 25 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 1
      , category = Nothing
      }
    , { label = [ "C" ]
      , value = [ "UC" ]
      , translate = Just { x = -10, y = 20 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Nothing
      }
    , { label = [ "A" ]
      , value = [ "UA" ]
      , translate = Just { x = -20, y = 20 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 3
      , category = Nothing
      }
    , { label = [ "G" ]
      , value = [ "UG" ]
      , translate = Just { x = -20, y = 10 }
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 4
      , category = Nothing
      }
    ]


getCytosineRadialThirdSegment : List BaseSlice
getCytosineRadialThirdSegment =
    let
        startAngle =
            0

        endAngle =
            90

        segmentAngle =
            (endAngle - startAngle) / 4

        subSegmentAngle =
            segmentAngle / 2
    in
    [ { label = [ "U", "C", "A", "G" ]
      , value = [ "CUU", "CUC", "CUA", "CUG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 1
      , category = Nothing
      }
    , { label = [ "U", "C", "A", "G" ]
      , value = [ "CCU", "CCC", "CCA", "CCG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Nothing
      }
    , { label = [ "U", "C" ]
      , value = [ "CAU", "CAC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 2 + subSegmentAngle * 1
      , category = Nothing
      }
    , { label = [ "A", "G" ]
      , value = [ "CAA", "CAG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2 + subSegmentAngle * 1
      , endAngle = startAngle + segmentAngle * 3
      , category = Nothing
      }
    , { label = [ "U", "C", "A", "G" ]
      , value = [ "CGU", "CGC", "CGA", "CGG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 4
      , category = Nothing
      }
    ]


getAdenineRadialThirdSegment : List BaseSlice
getAdenineRadialThirdSegment =
    let
        startAngle =
            90

        endAngle =
            180

        segmentAngle =
            (endAngle - startAngle) / 4

        mediumSegmentAngle =
            segmentAngle / 2

        twoThirdsSegmentAngle =
            (segmentAngle * 2) / 3
    in
    [ { label = [ "U", "C", "A" ]
      , value = [ "AUU", "AUC", "AUA" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 0 + twoThirdsSegmentAngle
      , category = Nothing
      }
    , { label = [ "G" ]
      , value = [ "AUG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0 + twoThirdsSegmentAngle
      , endAngle = startAngle + segmentAngle * 1
      , category = Nothing
      }
    , { label = [ "U", "C", "A", "G" ]
      , value = [ "ACU", "ACC", "ACA", "ACG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Nothing
      }
    , { label = [ "U", "C" ]
      , value = [ "AAU", "AAC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , category = Nothing
      }
    , { label = [ "A", "G" ]
      , value = [ "AAA", "AAG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 3
      , category = Nothing
      }
    , { label = [ "U", "C" ]
      , value = [ "AGU", "AGC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle
      , category = Nothing
      }
    , { label = [ "A", "G" ]
      , value = [ "AGA", "AGG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 4
      , category = Nothing
      }
    ]


getGuanineRadialThirdSegment : List BaseSlice
getGuanineRadialThirdSegment =
    let
        startAngle =
            180

        endAngle =
            270

        segmentAngle =
            (endAngle - startAngle) / 4

        mediumSegmentAngle =
            segmentAngle / 2
    in
    [ { label = [ "U", "C", "A", "G" ]
      , value = [ "GUU", "GUC", "GUA", "GUG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 1
      , category = Nothing
      }
    , { label = [ "U", "C", "A", "G" ]
      , value = [ "GCU", "GCC", "GCA", "GCG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Nothing
      }
    , { label = [ "U", "C" ]
      , value = [ "GAU", "GAC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , category = Nothing
      }
    , { label = [ "A", "G" ]
      , value = [ "GAA", "GAG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 3
      , category = Nothing
      }
    , { label = [ "U", "C", "A", "G" ]
      , value = [ "GGU", "GGC", "GGA", "GGG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 4
      , category = Nothing
      }
    ]


getUracilRadialThirdSegment : List BaseSlice
getUracilRadialThirdSegment =
    let
        startAngle =
            270

        endAngle =
            360

        segmentAngle =
            (endAngle - startAngle) / 4

        mediumSegmentAngle =
            segmentAngle / 2

        quarterSegmentAngle =
            segmentAngle / 4
    in
    [ { label = [ "U", "C" ]
      , value = [ "UUU", "UUC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 0 + mediumSegmentAngle
      , category = Nothing
      }
    , { label = [ "A", "G" ]
      , value = [ "UUA", "UUG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 1
      , category = Nothing
      }
    , { label = [ "U", "C", "A", "G" ]
      , value = [ "UCU", "UCC", "UCA", "UCG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Nothing
      }
    , { label = [ "U", "C" ]
      , value = [ "UAU", "UAC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , category = Nothing
      }
    , { label = [ "A", "G" ]
      , value = [ "UAA", "UAG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 3
      , category = Nothing
      }
    , { label = [ "U", "C" ]
      , value = [ "UGU", "UGC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle
      , category = Nothing
      }
    , { label = [ "A" ]
      , value = [ "UGA" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle + quarterSegmentAngle
      , category = Nothing
      }
    , { label = [ "G" ]
      , value = [ "UGG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle + quarterSegmentAngle
      , endAngle = startAngle + segmentAngle * 4
      , category = Nothing
      }
    ]


getCytosineRadialFourthSegment : List BaseSlice
getCytosineRadialFourthSegment =
    let
        startAngle =
            0

        endAngle =
            90

        segmentAngle =
            (endAngle - startAngle) / 4

        subSegmentAngle =
            segmentAngle / 2
    in
    [ { label = [ "leucine" ]
      , value = [ "CUU", "CUC", "CUA", "CUG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 1
      , category = Just NonPolar
      }
    , { label = [ "proline" ]
      , value = [ "CCU", "CCC", "CCA", "CCG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Just Special
      }
    , { label = [ "histidine" ]
      , value = [ "CAU", "CAC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 2 + subSegmentAngle * 1
      , category = Just PositivelyCharged
      }
    , { label = [ "glutamine" ]
      , value = [ "CAA", "CAG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2 + subSegmentAngle * 1
      , endAngle = startAngle + segmentAngle * 3
      , category = Just PolarUncharged
      }
    , { label = [ "arginine" ]
      , value = [ "CGU", "CGC", "CGA", "CGG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 4
      , category = Just PositivelyCharged
      }
    ]


getAdenineRadialFourthSegment : List BaseSlice
getAdenineRadialFourthSegment =
    let
        startAngle =
            90

        endAngle =
            180

        segmentAngle =
            (endAngle - startAngle) / 4

        mediumSegmentAngle =
            segmentAngle / 2

        twoThirdsSegmentAngle =
            (segmentAngle * 2) / 3
    in
    [ { label = [ "isoleucine" ]
      , value = [ "AUU", "AUC", "AUA" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 0 + twoThirdsSegmentAngle
      , category = Just NonPolar
      }
    , { label = [ "methionine" ]
      , value = [ "AUG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0 + twoThirdsSegmentAngle
      , endAngle = startAngle + segmentAngle * 1
      , category = Just NonPolar
      }
    , { label = [ "threonine" ]
      , value = [ "ACU", "ACC", "ACA", "ACG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Just PolarUncharged
      }
    , { label = [ "asparagine" ]
      , value = [ "AAU", "AAC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , category = Just PolarUncharged
      }
    , { label = [ "lysine" ]
      , value = [ "AAA", "AAG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 3
      , category = Just PositivelyCharged
      }
    , { label = [ "serine" ]
      , value = [ "AGU", "AGC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle
      , category = Just PolarUncharged
      }
    , { label = [ "arginine" ]
      , value = [ "AGA", "AGG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 4
      , category = Just PositivelyCharged
      }
    ]


getGuanineRadialFourthSegment : List BaseSlice
getGuanineRadialFourthSegment =
    let
        startAngle =
            180

        endAngle =
            270

        segmentAngle =
            (endAngle - startAngle) / 4

        mediumSegmentAngle =
            segmentAngle / 2
    in
    [ { label = [ "valine" ]
      , value = [ "GUU", "GUC", "GUA", "GUG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 1
      , category = Just NonPolar
      }
    , { label = [ "alanine" ]
      , value = [ "GCU", "GCC", "GCA", "GCG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Just NonPolar
      }
    , { label = [ "aspartic acid" ]
      , value = [ "GAU", "GAC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , category = Just NegativelyCharged
      }
    , { label = [ "glutamic acid" ]
      , value = [ "GAA", "GAG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 3
      , category = Just NegativelyCharged
      }
    , { label = [ "glycine" ]
      , value = [ "GGU", "GGC", "GGA", "GGG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 4
      , category = Just Special
      }
    ]


getUracilRadialFourthSegment : List BaseSlice
getUracilRadialFourthSegment =
    let
        startAngle =
            270

        endAngle =
            360

        segmentAngle =
            (endAngle - startAngle) / 4

        mediumSegmentAngle =
            segmentAngle / 2

        quarterSegmentAngle =
            segmentAngle / 4
    in
    [ { label = [ "phenylalanine" ]
      , value = [ "UUU", "UUC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0
      , endAngle = startAngle + segmentAngle * 0 + mediumSegmentAngle
      , category = Just NonPolar
      }
    , { label = [ "leucine" ]
      , value = [ "UUA", "UUG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 0 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 1
      , category = Just NonPolar
      }
    , { label = [ "serine" ]
      , value = [ "UCU", "UCC", "UCA", "UCG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 1
      , endAngle = startAngle + segmentAngle * 2
      , category = Just PolarUncharged
      }
    , { label = [ "tyrosine" ]
      , value = [ "UAU", "UAC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2
      , endAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , category = Just NonPolar
      }
    , { label = [ "STOP" ]
      , value = [ "UAA", "UAG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 2 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 3
      , category = Just Stop
      }
    , { label = [ "cysteine" ]
      , value = [ "UGU", "UGC" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3
      , endAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle
      , category = Just Special
      }
    , { label = [ "STOP" ]
      , value = [ "UGA" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle
      , endAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle + quarterSegmentAngle
      , category = Just Stop
      }
    , { label = [ "tryptophan" ]
      , value = [ "UGG" ]
      , translate = Nothing
      , isActive = False
      , startAngle = startAngle + segmentAngle * 3 + mediumSegmentAngle + quarterSegmentAngle
      , endAngle = startAngle + segmentAngle * 4
      , category = Just NonPolar
      }
    ]
