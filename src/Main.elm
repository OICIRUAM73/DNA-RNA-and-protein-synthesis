module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Browser.Navigation
import Canvas exposing (Renderable, arc, lineTo, path, rect, shapes, text)
import Canvas.Settings exposing (fill, stroke)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Canvas.Settings.Line exposing (lineWidth)
import Canvas.Settings.Text exposing (TextAlign(..), TextBaseLine(..), align, baseLine, font)
import Color
import Element exposing (DeviceClass(..))
import Element.Background as Background
import Element.Border
import Element.Input as Input
import FeatherIcons
import Html
import Html.Attributes exposing (style)
import Model
    exposing
        ( BaseSlice
        , ProteinCategory(..)
        , colorPalette
        , getAdenineRadialFourthSegment
        , getAdenineRadialSecondSegment
        , getAdenineRadialThirdSegment
        , getCytosineRadialFourthSegment
        , getCytosineRadialSecondSegment
        , getCytosineRadialThirdSegment
        , getFirstRadialBaseSegment
        , getGuanineRadialFourthSegment
        , getGuanineRadialSecondSegment
        , getGuanineRadialThirdSegment
        , getUracilRadialFourthSegment
        , getUracilRadialSecondSegment
        , getUracilRadialThirdSegment
        )
import Process
import String.Extra
import Task
import Url
import Widget
import Widget.Material as Material


type Msg
    = Pass
    | MsgChangeWindowSize Int Int
    | InputText String
    | Submit
    | MsgGetRNA
    | MsgGetStartingFrame
    | MsgGetNextFrame ( Int, String )
    | MsgUnderlineNextCodon ( Int, String )
    | MsgLookForFirstCodonSegment String ( Int, String )
    | MsgLookForSecondCodonSegment String ( Int, String )
    | MsgLookForThirdCodonSegment String ( Int, String )
    | MsgGetCurrentCodonResult String ( Int, String )
    | MsgCheckResult String ( Int, String )
    | MsgFillFirstSegment
    | MsgFillSecondSegment
    | MsgFillThirdSegment
    | MsgFillFourthSegment
    | Frame Float
    | ShowAllSteps
    | HideAllSteps


delayNextStep : Msg -> Cmd Msg
delayNextStep msg =
    Process.sleep 1000
        |> Task.perform (\_ -> msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgChangeWindowSize width height ->
            let
                newModel =
                    { model | windowSize = { width = width, height = height } }
            in
            ( newModel, Cmd.none )

        InputText newValue ->
            ( { model | inputText = newValue }
            , Cmd.none
            )

        Submit ->
            let
                input =
                    String.toUpper model.inputText

                newInputText =
                    String.filter (\c -> List.member c [ 'A', 'T', 'G', 'C' ]) input

                frameWidth =
                    round <| 40 + (toFloat (String.length newInputText) * 18)

                currentMessage =
                    "We look for the RNA strand"
            in
            ( { model
                | dnaChain = newInputText
                , frameWidth = frameWidth
                , workingFrame = []
                , message = currentMessage
                , steps =
                    model.steps
                        ++ [ "The DNA strand is: " ++ model.inputText
                           , currentMessage
                           ]
              }
            , delayNextStep MsgGetRNA
            )

        MsgGetRNA ->
            let
                getBase base =
                    case base of
                        'A' ->
                            'U'

                        'T' ->
                            'A'

                        'G' ->
                            'C'

                        'C' ->
                            'G'

                        _ ->
                            '_'

                currentMessage =
                    "We look for the starting codon: 'AUG'"

                rnaChain =
                    String.map getBase model.dnaChain
            in
            ( { model
                | rnaChain = rnaChain
                , workingFrame = []
                , message = currentMessage
                , steps =
                    model.steps
                        ++ [ currentMessage
                           ]
              }
            , delayNextStep MsgGetStartingFrame
            )

        MsgGetStartingFrame ->
            let
                startIndex =
                    List.head <| String.indexes "AUG" model.rnaChain

                currentMessage =
                    "We take the first 3 bases from the starting codon"

                newModelAndCmd =
                    case Maybe.map (\i -> ( i, String.dropLeft i model.rnaChain )) startIndex of
                        Just newWorkingFrame ->
                            ( { model
                                | workingFrame = [ newWorkingFrame ]
                                , lastFrame = newWorkingFrame
                                , message = currentMessage
                                , steps = model.steps ++ [ currentMessage ]
                              }
                            , delayNextStep <| MsgUnderlineNextCodon newWorkingFrame
                            )

                        Nothing ->
                            ( model
                            , Cmd.none
                            )
            in
            newModelAndCmd

        MsgUnderlineNextCodon currentFrame ->
            let
                nextCodon =
                    String.left 3 <| Tuple.second currentFrame

                currentMessage =
                    "The next codon doesn't have minimun 3 bases. The process ends."

                newModelAndCmd =
                    if String.length nextCodon == 3 then
                        ( { model
                            | underlineNextCodon = True
                          }
                        , delayNextStep <| MsgLookForFirstCodonSegment nextCodon currentFrame
                        )

                    else
                        ( { model
                            | message = currentMessage
                            , steps = model.steps ++ [ currentMessage ]
                          }
                        , Cmd.none
                        )
            in
            newModelAndCmd

        MsgLookForFirstCodonSegment nextCodon currentFrame ->
            ( { model
                | underlineFirstNextCodon = True
                , currentCodon = Just <| String.left 1 nextCodon
              }
            , delayNextStep <| MsgLookForSecondCodonSegment nextCodon currentFrame
            )

        MsgLookForSecondCodonSegment nextCodon currentFrame ->
            ( { model
                | underlineSecondNextCodon = True
                , currentCodon = Just <| String.left 2 nextCodon
              }
            , delayNextStep <| MsgLookForThirdCodonSegment nextCodon currentFrame
            )

        MsgLookForThirdCodonSegment nextCodon currentFrame ->
            ( { model
                | underlineThirdNextCodon = True
                , currentCodon = Just nextCodon
              }
            , delayNextStep <| MsgGetCurrentCodonResult nextCodon currentFrame
            )

        MsgGetCurrentCodonResult nextCodon currentFrame ->
            let
                maybeCodonAminoAcid =
                    Maybe.andThen (\e -> List.head e.label)
                        (List.head <|
                            List.filter
                                (\e -> List.member nextCodon e.value)
                                model.fourthSegment
                        )

                newModelAndMsg =
                    case maybeCodonAminoAcid of
                        Just aminoAcidFound ->
                            let
                                currentMessage =
                                    "Protein found: "
                                        ++ aminoAcidFound
                                        ++ " for the codon: '"
                                        ++ nextCodon
                                        ++ "'"
                            in
                            ( { model
                                | message = currentMessage
                                , steps = model.steps ++ [ currentMessage ]
                                , result = model.result ++ [ aminoAcidFound ]
                              }
                            , delayNextStep <| MsgCheckResult aminoAcidFound currentFrame
                            )

                        Nothing ->
                            let
                                currentMessage =
                                    "We did not find a protein for the codon. The process ends."
                            in
                            ( { model
                                | message = currentMessage
                                , steps = model.steps ++ [ currentMessage ]
                              }
                            , Cmd.none
                            )
            in
            newModelAndMsg

        MsgCheckResult lastResult lastFrame ->
            let
                newModelAndCmd =
                    if lastResult == "STOP" then
                        let
                            currentMessage =
                                "The result is 'STOP' so the process ends."
                        in
                        ( { model
                            | message = currentMessage
                            , steps = model.steps ++ [ currentMessage ]
                            , showStepsResume = True
                          }
                        , Cmd.none
                        )

                    else
                        let
                            currentMessage =
                                "We take the next 3 bases"
                        in
                        ( { model
                            | currentCodon = Nothing
                            , message = currentMessage
                            , steps = model.steps ++ [ currentMessage ]
                          }
                        , delayNextStep <| MsgGetNextFrame lastFrame
                        )
            in
            newModelAndCmd

        MsgGetNextFrame lastFrame ->
            let
                ( index, frame ) =
                    lastFrame

                nextFrame =
                    ( index + 3, String.dropLeft 3 frame )

                nextWorkingFrame =
                    model.workingFrame
                        ++ [ nextFrame ]

                frameHeight =
                    round <| 130 + (toFloat (List.length nextWorkingFrame - 1) * 50)

                newModelAndCmd =
                    ( { model
                        | workingFrame =
                            nextWorkingFrame
                        , underlineNextCodon = False
                        , underlineFirstNextCodon = False
                        , underlineSecondNextCodon = False
                        , underlineThirdNextCodon = False
                        , frameHeight = frameHeight
                      }
                    , delayNextStep <| MsgUnderlineNextCodon nextFrame
                    )
            in
            newModelAndCmd

        Frame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )

        MsgFillFirstSegment ->
            ( { model
                | firstSegment = getFirstRadialBaseSegment
              }
            , delayNextStep MsgFillSecondSegment
            )

        MsgFillSecondSegment ->
            ( { model
                | secondSegment =
                    getCytosineRadialSecondSegment
                        ++ getAdenineRadialSecondSegment
                        ++ getGuanineRadialSecondSegment
                        ++ getUracilRadialSecondSegment
              }
            , delayNextStep MsgFillThirdSegment
            )

        MsgFillThirdSegment ->
            ( { model
                | thirdSegment =
                    getCytosineRadialThirdSegment
                        ++ getAdenineRadialThirdSegment
                        ++ getGuanineRadialThirdSegment
                        ++ getUracilRadialThirdSegment
              }
            , delayNextStep MsgFillFourthSegment
            )

        MsgFillFourthSegment ->
            ( { model
                | fourthSegment =
                    getCytosineRadialFourthSegment
                        ++ getAdenineRadialFourthSegment
                        ++ getGuanineRadialFourthSegment
                        ++ getUracilRadialFourthSegment
              }
            , Cmd.none
            )

        ShowAllSteps ->
            ( { model | showModal = True }
            , Cmd.none
            )

        HideAllSteps ->
            ( { model | showModal = False }
            , Cmd.none
            )

        Pass ->
            ( model
            , Cmd.none
            )


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navigationKey =
    let
        model =
            initModel flags url navigationKey
    in
    ( model
    , initCmd model
    )


type alias Model =
    { windowSize : WindowSize
    , count : Float
    , firstSegment : List BaseSlice
    , secondSegment : List BaseSlice
    , thirdSegment : List BaseSlice
    , fourthSegment : List BaseSlice
    , inputText : String
    , dnaChain : String
    , rnaChain : String
    , workingFrame : List ( Int, String )
    , lastFrame : ( Int, String )
    , currentCodon : Maybe String
    , result : List String
    , message : String
    , steps : List String
    , underlineNextCodon : Bool
    , underlineFirstNextCodon : Bool
    , underlineSecondNextCodon : Bool
    , underlineThirdNextCodon : Bool
    , frameHeight : Int
    , frameWidth : Int
    , showModal : Bool
    , showStepsResume : Bool
    }


initModel : Flags -> Url.Url -> Browser.Navigation.Key -> Model
initModel flags url navigationKey =
    { windowSize = flags.windowSize
    , count = 0
    , firstSegment = []
    , secondSegment = []
    , thirdSegment = []
    , fourthSegment = []
    , dnaChain = ""
    , rnaChain = ""
    , inputText = "ATGCATGCTACGTCGCTAGTCTGTGTCGTAGCCATACGTGCACTGAAAGGAGTACTCGACTC"
    , workingFrame = []
    , lastFrame = ( 0, "" )
    , currentCodon = Nothing
    , result = []
    , message = ""
    , steps = []
    , underlineNextCodon = False
    , underlineFirstNextCodon = False
    , underlineSecondNextCodon = False
    , underlineThirdNextCodon = False
    , frameHeight = 130
    , frameWidth = 100
    , showModal = False
    , showStepsResume = False
    }


type alias Flags =
    { windowSize : WindowSize
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


initCmd : Model -> Cmd Msg
initCmd model =
    delayNextStep MsgFillFirstSegment


view : Model -> Browser.Document Msg
view model =
    { title = "Tus postulantes · Talenteca"
    , body =
        [ viewStylish model
        ]
    }


viewStylish : Model -> Html.Html Msg
viewStylish model =
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        ((Background.color <| Element.rgb255 244 244 244)
            :: (if model.showModal then
                    let
                        style =
                            Material.alertDialog colorPalette
                    in
                    Widget.singleModal
                        [ { onDismiss = Just HideAllSteps
                          , content =
                                Element.column
                                    ([ Element.centerX
                                     , Element.centerY
                                     , Element.spacing 10
                                     ]
                                        ++ style.elementColumn
                                    )
                                    [ Element.el style.content.title.contentText <| Element.text "Translation steps"
                                    , Element.column
                                        (style.content.text.contentText
                                            ++ [ Element.spacing 10
                                               , Element.height
                                                    (Element.fill
                                                        |> Element.minimum (round (toFloat model.windowSize.height / 2))
                                                    )
                                               , Element.scrollbarY
                                               ]
                                        )
                                        (List.map
                                            (\e ->
                                                Element.row [ Element.spacing 8 ] <|
                                                    [ FeatherIcons.chevronRight
                                                        |> FeatherIcons.withSize 15
                                                        |> FeatherIcons.toHtml []
                                                        |> Element.html
                                                        |> Element.el []
                                                    , Element.paragraph
                                                        [ Element.width Element.fill
                                                        , Element.alignTop
                                                        ]
                                                      <|
                                                        [ Element.el
                                                            [ Element.htmlAttribute <|
                                                                Html.Attributes.style "word-break" "break-word"
                                                            ]
                                                          <|
                                                            Element.text e
                                                        ]
                                                    ]
                                            )
                                            model.steps
                                        )
                                    , Element.row
                                        ([ Element.alignRight
                                         , Element.width <| Element.shrink
                                         ]
                                            ++ style.content.buttons.elementRow
                                        )
                                        ({ text = "OK"
                                         , onPress = Just HideAllSteps
                                         }
                                            |> Widget.textButton style.content.buttons.content.accept
                                            |> List.singleton
                                        )
                                    ]
                          }
                        ]

                else
                    []
               )
        )
    <|
        viewElement model


viewElement : Model -> Element.Element Msg
viewElement model =
    let
        firstSegmentChain =
            Maybe.withDefault "" <| Maybe.map (\c -> String.left 1 c) model.currentCodon

        secondSegmentChain =
            Maybe.withDefault "" <| Maybe.map (\c -> String.left 2 c) model.currentCodon

        thirdSegmentChain =
            Maybe.withDefault "" <| Maybe.map (\c -> String.left 3 c) model.currentCodon
    in
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.column
            [ Element.width <|
                (Element.fill
                    |> Element.maximum (round (toFloat model.windowSize.width / 2 - 2))
                )
            , Element.height Element.fill
            , Element.spaceEvenly
            ]
            [ Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 20
                , Element.spacing 20
                ]
                [ Element.el [ Element.centerX ] <| Element.text "DNA TRANSLATION"
                , Element.row
                    [ Element.spacing 10
                    , Element.width Element.fill
                    ]
                    [ let
                        textStyle =
                            Material.textInput colorPalette

                        newStyle =
                            { textStyle | elementRow = textStyle.elementRow ++ [ Element.width Element.fill ] }
                      in
                      Widget.searchInput newStyle
                        { chips = []
                        , text = model.inputText
                        , placeholder = Just <| Input.placeholder [] <| Element.text "AAGUUCAA"
                        , label = "Enter your DNA Secuence"
                        , onChange = InputText
                        }
                    , Widget.textButton (Material.containedButton colorPalette)
                        { text = "Submit"
                        , onPress = Just Submit
                        }
                    ]
                , if model.dnaChain /= "" then
                    Element.paragraph [ Element.width Element.fill ]
                        [ Element.text "DNA Strand: ", Element.text model.dnaChain ]

                  else
                    Element.none
                , if model.rnaChain /= "" then
                    Element.paragraph [ Element.width Element.fill ]
                        [ Element.text "Protein Result: "
                        , Element.text <|
                            String.Extra.toTitleCase <|
                                String.join ", " model.result
                        ]

                  else
                    Element.none
                , if String.length model.message > 0 then
                    Element.text model.message

                  else
                    Element.none
                , if model.showStepsResume then
                    Element.el [ Element.centerX ] <|
                        Widget.textButton (Material.containedButton colorPalette)
                            { text = "Show all steps"
                            , onPress = Just ShowAllSteps
                            }

                  else
                    Element.none
                ]
            , Element.el
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.scrollbarY
                , Element.scrollbarX
                ]
              <|
                Element.html <|
                    Canvas.toHtml
                        ( model.frameWidth, model.frameHeight )
                        []
                        (clearScreen model.frameWidth model.frameHeight
                            :: renderDnaCompleteFrameSequence model
                            ++ renderRnaCompleteFrameSequence model
                            ++ renderFrameSequence model
                        )
            ]
        , Element.el
            [ Element.width <|
                (Element.fill
                    |> Element.maximum (round (toFloat model.windowSize.width / 2))
                )
            , Element.height Element.fill
            , Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 2 }
            , Element.Border.color <| Element.rgb255 200 200 200
            ]
          <|
            Element.html <|
                let
                    canvasWidth =
                        round <| toFloat model.windowSize.width / 2

                    canvasHeight =
                        model.windowSize.height - 3

                    maxRadius =
                        (if canvasWidth < canvasHeight then
                            toFloat canvasWidth / 2

                         else
                            toFloat canvasHeight / 2
                        )
                            - 20

                    fourthCirleRadius =
                        if maxRadius > 350 then
                            maxRadius

                        else
                            maxRadius - 50

                    thirdCirleRadius =
                        if maxRadius > 350 then
                            maxRadius - (maxRadius / 4)

                        else
                            fourthCirleRadius - 100

                    secondCirleRadius =
                        if maxRadius > 350 then
                            maxRadius - 2 * (maxRadius / 4)

                        else
                            thirdCirleRadius - 50

                    firstCirleRadius =
                        if maxRadius > 350 then
                            maxRadius - 3 * (maxRadius / 4)

                        else
                            secondCirleRadius - 50
                in
                Canvas.toHtml
                    ( canvasWidth, canvasHeight )
                    []
                    (clearScreen canvasWidth canvasHeight
                        :: renderInnerCircle model thirdSegmentChain model.fourthSegment 4 thirdCirleRadius fourthCirleRadius 15
                        ++ renderInnerCircle model thirdSegmentChain model.thirdSegment 3 secondCirleRadius thirdCirleRadius 12
                        ++ renderInnerCircle model secondSegmentChain model.secondSegment 2 firstCirleRadius secondCirleRadius 20
                        ++ renderInnerCircle model firstSegmentChain model.firstSegment 1 0 firstCirleRadius 30
                    )
        ]


renderDnaCompleteFrameSequence : Model -> List Renderable
renderDnaCompleteFrameSequence model =
    let
        renderText index t =
            text
                [ font { size = 22, family = "sans-serif" }
                , align Center
                , baseLine Middle
                , stroke Color.black
                , fill Color.black
                ]
                ( 20 + (toFloat index * 18)
                , 20
                )
                (String.fromChar t)

        renderTextChain frame =
            List.indexedMap renderText (String.toList frame)
    in
    if model.dnaChain /= "" then
        renderTextChain model.dnaChain

    else
        []


renderRnaCompleteFrameSequence : Model -> List Renderable
renderRnaCompleteFrameSequence model =
    let
        startIndex =
            Maybe.withDefault -1 <| List.head <| String.indexes "AUG" model.rnaChain

        fontColor index =
            if index >= startIndex && index < startIndex + 3 then
                Color.red

            else
                Color.black

        paddingLeft =
            20

        paddingTop =
            28

        arrowWidth =
            18

        renderText index t =
            text
                [ font { size = 22, family = "sans-serif" }
                , align Center
                , baseLine Middle
                , stroke <| fontColor index
                , fill <| fontColor index
                ]
                ( 20 + (toFloat index * 18)
                , 50
                )
                (String.fromChar t)

        renderArrow index _ =
            shapes [ stroke Color.black, lineWidth 1 ]
                [ path ( paddingLeft + (toFloat index * arrowWidth), paddingTop )
                    [ lineTo ( paddingLeft + (toFloat index * arrowWidth), paddingTop + 10 )
                    ]
                ]

        renderTextChain frame =
            List.indexedMap renderText (String.toList frame)
                ++ List.indexedMap renderArrow (String.toList frame)
    in
    if model.rnaChain /= "" then
        renderTextChain model.rnaChain

    else
        []


renderFrameSequence : Model -> List Renderable
renderFrameSequence model =
    let
        paddingLeft =
            20

        paddingTop =
            60

        arrowWidth =
            18

        arrowHeight =
            50

        totalFrames =
            List.length model.workingFrame

        renderTextChainAndArrow verticalIndex ( position, workingFrame ) =
            let
                renderArrow =
                    shapes [ stroke Color.black, lineWidth 2 ]
                        [ path ( paddingLeft + (toFloat position * arrowWidth), paddingTop + (toFloat verticalIndex * arrowHeight) )
                            [ lineTo ( paddingLeft + (toFloat position * arrowWidth), paddingTop + 20 + (toFloat verticalIndex * arrowHeight) )
                            , lineTo ( paddingLeft + 10 + (toFloat position * arrowWidth), paddingTop + 10 + (toFloat verticalIndex * arrowHeight) )
                            , lineTo ( paddingLeft + (toFloat position * arrowWidth), paddingTop + 20 + (toFloat verticalIndex * arrowHeight) )
                            , lineTo ( paddingLeft - 10 + (toFloat position * arrowWidth), paddingTop + 10 + (toFloat verticalIndex * arrowHeight) )
                            ]
                        ]

                renderText index t =
                    text
                        ([ font { size = 22, family = "sans-serif" }
                         , align Center
                         , baseLine Middle
                         ]
                            ++ (if verticalIndex == totalFrames - 1 && model.underlineNextCodon then
                                    if index == 0 && model.underlineFirstNextCodon then
                                        [ stroke Color.blue, fill Color.blue ]

                                    else if index == 1 && model.underlineSecondNextCodon then
                                        [ stroke Color.blue, fill Color.blue ]

                                    else if index == 2 && model.underlineThirdNextCodon then
                                        [ stroke Color.blue, fill Color.blue ]

                                    else if index < 3 then
                                        [ stroke Color.blue ]

                                    else
                                        []

                                else
                                    []
                               )
                        )
                        ( paddingLeft + (toFloat position * 18) + (toFloat index * 18)
                        , 100 + (toFloat verticalIndex * 50)
                        )
                        (String.fromChar t)

                renderTextChain =
                    List.indexedMap renderText (String.toList workingFrame)
            in
            renderTextChain ++ [ renderArrow ]
    in
    List.concat <| List.indexedMap renderTextChainAndArrow model.workingFrame


renderInnerCircle : Model -> String -> List BaseSlice -> Int -> Float -> Float -> Int -> List Canvas.Renderable
renderInnerCircle model activeSegment segmentList segment startRadius radius fontSize =
    let
        centerX =
            toFloat model.windowSize.width / 4

        centerY =
            toFloat model.windowSize.height / 2

        center =
            ( centerX, centerY )

        middleRadius =
            if segment < 3 then
                radius

            else
                (startRadius + radius) / 2

        categoryColor category =
            case category of
                Just NonPolar ->
                    Color.rgb255 159 187 185

                Just PolarUncharged ->
                    Color.rgb255 197 169 194

                Just PositivelyCharged ->
                    Color.rgb255 167 187 235

                Just NegativelyCharged ->
                    Color.rgb255 237 178 133

                Just Special ->
                    Color.rgb255 192 198 135

                Just Stop ->
                    Color.rgb255 199 139 129

                Nothing ->
                    Color.white

        viewEl index element =
            let
                isActive =
                    List.member activeSegment element.value

                sliceColor =
                    if isActive then
                        Color.blue

                    else
                        categoryColor element.category

                labelColor =
                    if isActive then
                        [ fill Color.white
                        , stroke Color.white
                        ]

                    else
                        [ fill Color.black ]

                totalLabels =
                    toFloat <| List.length element.label

                labelAngleSize =
                    (element.endAngle - element.startAngle) / (totalLabels + 1)

                pointX angle =
                    centerX + middleRadius * cos (degrees angle)

                startPointX =
                    pointX element.startAngle

                endPointX =
                    pointX element.endAngle

                pointY angle =
                    centerY + middleRadius * sin (degrees angle)

                startPointY =
                    pointY element.startAngle

                endPointY =
                    pointY element.endAngle

                beginPointX =
                    if startPointX < endPointX then
                        startPointX

                    else
                        endPointX

                pointXSize =
                    abs (startPointX - endPointX) / (totalLabels + 1)

                beginPointY =
                    if startPointY < endPointY then
                        startPointY

                    else
                        endPointY

                pointYSize =
                    abs (startPointY - endPointY) / (totalLabels + 1)

                labelView labelIndex label =
                    let
                        incrementalX =
                            if startPointX < endPointX then
                                toFloat labelIndex + 1

                            else
                                totalLabels - toFloat labelIndex

                        incrementalY =
                            if startPointY < endPointY then
                                toFloat labelIndex + 1

                            else
                                totalLabels - toFloat labelIndex

                        labelX =
                            beginPointX + incrementalX * pointXSize

                        labelY =
                            beginPointY + incrementalY * pointYSize

                        textCenterX =
                            labelX

                        textCenterY =
                            labelY

                        middleAngle =
                            element.startAngle + (labelAngleSize * toFloat (labelIndex + 1))

                        rotateAngle =
                            if middleAngle > 90 && middleAngle < 270 then
                                180 + middleAngle

                            else
                                middleAngle

                        translateText =
                            case element.translate of
                                Just translateEl ->
                                    [ translate translateEl.x translateEl.y ]

                                Nothing ->
                                    [ translate textCenterX textCenterY
                                    , rotate <| degrees rotateAngle
                                    , translate -textCenterX -textCenterY
                                    ]
                    in
                    text
                        ([ font { size = fontSize, family = "sans-serif" }
                         , align Center
                         , baseLine Middle
                         , transform translateText
                         ]
                            ++ labelColor
                        )
                        ( textCenterX
                        , textCenterY
                        )
                        label
            in
            [ renderPieSlice sliceColor
                center
                radius
                element.startAngle
                element.endAngle
            , renderPieSliceBorder Color.black
                center
                radius
                element.startAngle
                element.endAngle
            ]
                ++ List.indexedMap labelView element.label

        elements =
            List.indexedMap viewEl segmentList

        processedView =
            List.concat elements
    in
    processedView


renderPieSlice color (( x, y ) as center) radius startAngleC endAngleC =
    let
        startAngle =
            degrees startAngleC

        endAngle =
            degrees endAngleC
    in
    shapes [ fill color ]
        [ path center
            [ lineTo ( x + radius * cos startAngle, y + radius * sin startAngle )
            , lineTo ( x + radius * cos endAngle, y + radius * sin endAngle )
            , lineTo center
            ]
        , arc
            center
            radius
            { startAngle = startAngle
            , endAngle = endAngle
            , clockwise = True
            }
        ]


renderPieSliceBorder color (( x, y ) as center) radius startAngleC endAngleC =
    let
        startAngle =
            degrees startAngleC

        endAngle =
            degrees endAngleC
    in
    shapes [ stroke color, lineWidth 2 ]
        [ path center
            [ lineTo ( x + radius * cos startAngle, y + radius * sin startAngle )
            , lineTo center
            , lineTo ( x + radius * cos endAngle, y + radius * sin endAngle )
            , lineTo center
            ]
        , arc
            center
            radius
            { startAngle = startAngle
            , endAngle = endAngle
            , clockwise = True
            }
        ]


clearScreen : Int -> Int -> Canvas.Renderable
clearScreen width height =
    shapes [ fill <| Color.rgb255 244 244 244 ]
        [ rect ( 0, 0 )
            (toFloat width)
            (toFloat height)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize MsgChangeWindowSize



--onAnimationFrameDelta Frame


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlRequest =
    Pass


onUrlChange : Url.Url -> Msg
onUrlChange url =
    Pass
