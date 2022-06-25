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
import CustomColor exposing (colorPalette)
import Element exposing (DeviceClass(..))
import Element.Background as Background
import Element.Border
import Element.Font
import Element.Input as Input
import FeatherIcons
import Font
import Html
import Html.Attributes exposing (style)
import Model
    exposing
        ( BaseSlice
        , ProteinCategory(..)
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
    | MsgUnderlineFirstCodonSegment String ( Int, String )
    | MsgLookForFirstCodonSegment String ( Int, String )
    | MsgUnderlineSecondCodonSegment String ( Int, String )
    | MsgLookForSecondCodonSegment String ( Int, String )
    | MsgUnderlineThirdCodonSegment String ( Int, String )
    | MsgLookForThirdCodonSegment String ( Int, String )
    | MsgGetCurrentCodonResult String ( Int, String )
    | MsgCheckResult String ( Int, String )
    | MsgReset
    | MsgFillFirstSegment
    | MsgFillSecondSegment
    | MsgFillThirdSegment
    | MsgFillFourthSegment
    | Frame Float
    | ShowAllSteps
    | HideAllSteps
    | ShowCodonTableModal (Maybe Msg)
    | HideCodonTableModal


delayNextStep : Float -> Msg -> Cmd Msg
delayNextStep time msg =
    Process.sleep time
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
                    "Get the RNA strand from DNA"
            in
            ( { model
                | dnaChain = newInputText
                , frameWidth = frameWidth
                , workingFrame = []
                , message = currentMessage
                , processState = Running
                , steps =
                    model.steps
                        ++ [ "The DNA strand is: " ++ model.inputText
                           , currentMessage
                           ]
              }
            , delayNextStep 1500 MsgGetRNA
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
                    "Look for the starting codon: 'AUG'"

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
            , delayNextStep 1500 MsgGetStartingFrame
            )

        MsgGetStartingFrame ->
            let
                startIndex =
                    List.head <| String.indexes "AUG" model.rnaChain

                currentMessage =
                    "Take the first 3 bases from the starting codon"

                newModelAndCmd =
                    case Maybe.map (\i -> ( i, String.dropLeft i model.rnaChain )) startIndex of
                        Just newWorkingFrame ->
                            ( { model
                                | workingFrame = [ newWorkingFrame ]
                                , lastFrame = newWorkingFrame
                                , message = currentMessage
                                , steps = model.steps ++ [ currentMessage ]
                              }
                            , delayNextStep 1500 <| MsgUnderlineNextCodon newWorkingFrame
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

                newModelAndCmd =
                    if String.length nextCodon == 3 then
                        ( { model
                            | underlineNextCodon = True
                            , message = "Look for the first base in the internal region of the Radial Codon Table."
                            , steps = model.steps ++ [ "Look for the codon in the internal region of the Radial Codon Table." ]
                          }
                        , delayNextStep 1500 <| MsgUnderlineFirstCodonSegment nextCodon currentFrame
                        )

                    else
                        let
                            currentMessage =
                                "The next codon doesn't have minimun 3 bases. The process ends."
                        in
                        ( { model
                            | message = currentMessage
                            , processState = Finished
                            , steps = model.steps ++ [ currentMessage ]
                          }
                        , Cmd.none
                        )
            in
            newModelAndCmd

        MsgUnderlineFirstCodonSegment nextCodon currentFrame ->
            let
                nextMsg =
                    MsgLookForFirstCodonSegment nextCodon currentFrame
            in
            ( { model
                | underlineFirstNextCodon = True
                , message = "Look for the second base in the middle region of the Radial Codon Table."
              }
            , if model.isMobile then
                delayNextStep 1500 <| ShowCodonTableModal <| Just nextMsg

              else
                delayNextStep 1500 nextMsg
            )

        MsgLookForFirstCodonSegment nextCodon currentFrame ->
            ( { model
                | currentCodon = Just <| String.left 1 nextCodon
              }
            , delayNextStep 1500 <| MsgUnderlineSecondCodonSegment nextCodon currentFrame
            )

        MsgUnderlineSecondCodonSegment nextCodon currentFrame ->
            let
                nextMsg =
                    MsgLookForSecondCodonSegment nextCodon currentFrame
            in
            ( { model
                | underlineSecondNextCodon = True
                , showCodonTableRadialAsModal = False
                , message = "Look for the third base in the external region of the Radial Codon Table."
              }
            , if model.isMobile then
                delayNextStep 1500 <| ShowCodonTableModal <| Just nextMsg

              else
                delayNextStep 1500 nextMsg
            )

        MsgLookForSecondCodonSegment nextCodon currentFrame ->
            ( { model
                | currentCodon = Just <| String.left 2 nextCodon
              }
            , delayNextStep 1500 <| MsgUnderlineThirdCodonSegment nextCodon currentFrame
            )

        MsgUnderlineThirdCodonSegment nextCodon currentFrame ->
            let
                nextMsg =
                    MsgLookForThirdCodonSegment nextCodon currentFrame
            in
            ( { model
                | underlineThirdNextCodon = True
                , showCodonTableRadialAsModal = False
              }
            , if model.isMobile then
                delayNextStep 1500 <| ShowCodonTableModal <| Just nextMsg

              else
                delayNextStep 1500 nextMsg
            )

        MsgLookForThirdCodonSegment nextCodon currentFrame ->
            ( { model
                | currentCodon = Just nextCodon
              }
            , delayNextStep 1500 <| MsgGetCurrentCodonResult nextCodon currentFrame
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
                                , showCodonTableRadialAsModal = False
                                , steps = model.steps ++ [ currentMessage ]
                                , result = model.result ++ [ aminoAcidFound ]
                              }
                            , delayNextStep 1500 <| MsgCheckResult aminoAcidFound currentFrame
                            )

                        Nothing ->
                            let
                                currentMessage =
                                    "We did not find a protein for the codon. The process ends."
                            in
                            ( { model
                                | message = currentMessage
                                , processState = Finished
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
                                "The result is 'STOP', the process ends."
                        in
                        ( { model
                            | message = currentMessage
                            , steps = model.steps ++ [ currentMessage ]
                            , processState = Finished
                            , showStepsResume = True
                          }
                        , Cmd.none
                        )

                    else
                        let
                            currentMessage =
                                "We take the next codon ( 3 bases )"
                        in
                        ( { model
                            | currentCodon = Nothing
                            , message = currentMessage
                            , steps = model.steps ++ [ currentMessage ]
                          }
                        , delayNextStep 1500 <| MsgGetNextFrame lastFrame
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

                nextCodon =
                    String.left 3 <| Tuple.second nextFrame

                currentMessage =
                    "The next codon is: " ++ nextCodon

                newModelAndCmd =
                    ( { model
                        | workingFrame = nextWorkingFrame
                        , message = currentMessage
                        , steps = model.steps ++ [ currentMessage ]
                        , underlineNextCodon = False
                        , underlineFirstNextCodon = False
                        , underlineSecondNextCodon = False
                        , underlineThirdNextCodon = False
                        , frameHeight = frameHeight
                      }
                    , delayNextStep 1500 <| MsgUnderlineNextCodon nextFrame
                    )
            in
            newModelAndCmd

        MsgReset ->
            ( { model
                | dnaChain = ""
                , rnaChain = ""
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
                , showStepsResume = False
                , processState = Initial
              }
            , Cmd.none
            )

        Frame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )

        MsgFillFirstSegment ->
            ( { model
                | firstSegment = getFirstRadialBaseSegment
              }
            , delayNextStep 200 MsgFillSecondSegment
            )

        MsgFillSecondSegment ->
            ( { model
                | secondSegment =
                    getCytosineRadialSecondSegment
                        ++ getAdenineRadialSecondSegment
                        ++ getGuanineRadialSecondSegment
                        ++ getUracilRadialSecondSegment
              }
            , delayNextStep 200 MsgFillThirdSegment
            )

        MsgFillThirdSegment ->
            ( { model
                | thirdSegment =
                    getCytosineRadialThirdSegment
                        ++ getAdenineRadialThirdSegment
                        ++ getGuanineRadialThirdSegment
                        ++ getUracilRadialThirdSegment
              }
            , delayNextStep 200 MsgFillFourthSegment
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

        ShowCodonTableModal maybeNextMsg ->
            ( { model | showCodonTableRadialAsModal = True }
            , case maybeNextMsg of
                Just nextMsg ->
                    delayNextStep 1500 nextMsg

                Nothing ->
                    Cmd.none
            )

        HideCodonTableModal ->
            ( { model | showCodonTableRadialAsModal = False }
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
    , processState : ProcessState
    , isMobile : Bool
    , showCodonTableRadialAsModal : Bool
    }


type ProcessState
    = Initial
    | Running
    | Finished


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
    , processState = Initial
    , isMobile = flags.windowSize.width <= 600
    , showCodonTableRadialAsModal = False
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
    delayNextStep 0 MsgFillFirstSegment


view : Model -> Browser.Document Msg
view model =
    { title = "DNA Translation"
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
            :: (let
                    style =
                        Material.alertDialog colorPalette
                in
                if model.showModal then
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

                else if model.showCodonTableRadialAsModal then
                    Widget.singleModal
                        [ { onDismiss = Just HideCodonTableModal
                          , content =
                                Element.column
                                    ([ Element.centerX
                                     , Element.centerY
                                     , Element.spacing 10
                                     ]
                                        ++ style.elementColumn
                                        ++ [ Element.height <| Element.px <| model.windowSize.width
                                           ]
                                    )
                                    [ Element.el style.content.title.contentText <| Element.text "Codon Table"
                                    , viewCodonTable model
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
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        [ Element.column
            [ if model.isMobile then
                Element.width <|
                    (Element.fill
                        |> Element.maximum (round (toFloat model.windowSize.width))
                    )

              else
                Element.width <|
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
                , Element.spacing 8
                ]
                [ Element.el (Font.headline colorPalette.primary ++ [ Element.centerX ]) <| Element.text "DNA TRANSLATION"
                , Element.wrappedRow
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
                    , let
                        label =
                            if model.processState == Finished then
                                "Reset"

                            else
                                "Submit"

                        onPressMsg =
                            case model.processState of
                                Initial ->
                                    Just Submit

                                Running ->
                                    Nothing

                                Finished ->
                                    Just MsgReset
                      in
                      Widget.textButton (Material.containedButton colorPalette)
                        { text = label
                        , onPress = onPressMsg
                        }
                    ]
                , if model.isMobile && model.processState == Initial then
                    Element.el
                        [ Element.centerX
                        ]
                    <|
                        Widget.textButton (Material.textButton colorPalette)
                            { text = "Show Codon Table"
                            , onPress = Just <| ShowCodonTableModal Nothing
                            }

                  else
                    Element.none
                , if model.dnaChain /= "" then
                    Element.paragraph
                        [ Element.width Element.fill ]
                        [ Element.el (Font.body colorPalette.primary) <| Element.text "DNA Strand: "
                        , Element.el (Font.body CustomColor.text) <| Element.text model.dnaChain
                        ]

                  else
                    Element.none
                , if model.rnaChain /= "" then
                    Element.paragraph
                        [ Element.width Element.fill ]
                        [ Element.el (Font.body colorPalette.primary) <| Element.text "RNA Strand: "
                        , Element.el (Font.body CustomColor.text) <| Element.text model.rnaChain
                        ]

                  else
                    Element.none
                , if model.rnaChain /= "" then
                    Element.column [ Element.width Element.fill, Element.spacing 5 ]
                        [ Element.el (Font.body colorPalette.primary) <| Element.text "Amino acid sequence: "
                        , Element.paragraph
                            (Font.body CustomColor.text
                                ++ (Element.width Element.fill
                                        :: (if model.processState == Finished then
                                                [ Element.Font.bold ]

                                            else
                                                []
                                           )
                                   )
                            )
                          <|
                            [ Element.text <|
                                String.Extra.toTitleCase <|
                                    String.join ", " model.result
                            ]
                        ]

                  else
                    Element.none
                , if String.length model.message > 0 then
                    Element.paragraph
                        [ Element.width Element.fill, Element.alignBottom ]
                        [ Element.el (Font.body colorPalette.primary) <| Element.text "Current Step: "
                        , Element.el (Font.body CustomColor.text ++ [ Element.Font.bold ]) <| Element.text model.message
                        ]

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
        , if model.isMobile then
            Element.none

          else
            Element.el
                [ Element.width <|
                    (Element.fill
                        |> Element.maximum (round (toFloat model.windowSize.width / 2))
                    )
                , Element.height Element.fill
                , Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 2 }
                , Element.Border.color <| Element.rgb255 200 200 200
                ]
            <|
                viewCodonTable model
        ]


viewCodonTable model =
    let
        firstSegmentChain =
            Maybe.withDefault "" <| Maybe.map (\c -> String.left 1 c) model.currentCodon

        secondSegmentChain =
            Maybe.withDefault "" <| Maybe.map (\c -> String.left 2 c) model.currentCodon

        thirdSegmentChain =
            Maybe.withDefault "" <| Maybe.map (\c -> String.left 3 c) model.currentCodon
    in
    Element.html <|
        let
            canvasWidth =
                if model.isMobile then
                    model.windowSize.width

                else
                    round <| toFloat model.windowSize.width / 2

            canvasHeight =
                if model.isMobile then
                    model.windowSize.width

                else
                    model.windowSize.height - 3

            maxRadius =
                (if canvasWidth < canvasHeight then
                    toFloat canvasWidth / 2

                 else
                    toFloat canvasHeight / 2
                )
                    - 10

            fourthCirleRadius =
                maxRadius

            thirdCirleRadius =
                maxRadius - 3 * (maxRadius / 9)

            secondCirleRadius =
                maxRadius - 5 * (maxRadius / 9)

            firstCirleRadius =
                maxRadius - 7 * (maxRadius / 9)

            fourthCircleFontSize =
                if model.isMobile then
                    10

                else if model.windowSize.width > 600 && model.windowSize.width <= 1050 then
                    10

                else if model.windowSize.width <= 1200 then
                    13

                else
                    15

            thirdCircleFontSize =
                if model.isMobile then
                    10

                else if model.windowSize.width > 600 && model.windowSize.width <= 900 then
                    10

                else
                    12

            secondCircleFontSize =
                if model.isMobile then
                    10

                else if model.windowSize.width > 600 && model.windowSize.width <= 900 then
                    10

                else
                    20

            firstCircleFontSize =
                if model.isMobile then
                    20

                else if model.windowSize.width > 600 && model.windowSize.width <= 900 then
                    20

                else
                    30
        in
        Canvas.toHtml
            ( canvasWidth, canvasHeight )
            []
            (clearScreen canvasWidth canvasHeight
                :: renderInnerCircle ( canvasWidth, canvasHeight ) thirdSegmentChain model.fourthSegment 4 thirdCirleRadius fourthCirleRadius fourthCircleFontSize
                ++ renderInnerCircle ( canvasWidth, canvasHeight ) thirdSegmentChain model.thirdSegment 3 secondCirleRadius thirdCirleRadius thirdCircleFontSize
                ++ renderInnerCircle ( canvasWidth, canvasHeight ) secondSegmentChain model.secondSegment 2 firstCirleRadius secondCirleRadius secondCircleFontSize
                ++ renderInnerCircle ( canvasWidth, canvasHeight ) firstSegmentChain model.firstSegment 1 0 firstCirleRadius firstCircleFontSize
            )


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


renderInnerCircle : ( Int, Int ) -> String -> List BaseSlice -> Int -> Float -> Float -> Int -> List Canvas.Renderable
renderInnerCircle ( canvasWidth, canvasHeight ) activeSegment segmentList segment startRadius radius fontSize =
    let
        centerX =
            toFloat canvasWidth / 2

        centerY =
            toFloat canvasHeight / 2

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
