port module Main exposing (main)

import Browser
import Browser.Events as Browser
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import File exposing (File)
import File.Select as Select
import Html exposing (Attribute, Html, text)
import Html.Events as Html
import Json.Decode as D
import List.Extra as List
import Midi exposing (Event(..), MidiEvent(..))
import Task


port play : { freq : Float, on : Bool } -> Cmd msg


port stop : () -> Cmd msg


type alias Model =
    { hover : Bool
    , files : Files
    , playing :
        Maybe
            { elapsed : Float
            , queued :
                List
                    { time : Int
                    , on : Bool
                    , key : Int
                    }
            }
    }


type alias Note =
    { name : NoteName
    , octave : Int
    , accidental : Accidental
    }


type NoteName
    = C
    | D
    | E
    | F
    | G
    | A
    | B


type Accidental
    = DoubleSharp
    | Sharp
    | Natural
    | None
    | Flat
    | DoubleFlat


type Files
    = Waiting
    | Loading
    | Loaded (List Bytes)


type Msg
    = Pick
    | DragEnter
    | DragLeave
    | GotFiles File (List File)
    | GotBytes (List Bytes)
    | Play Midi.File
    | Stop
    | Tick Float


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { hover = False
      , files = Waiting
      , playing = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Play midi ->
            let
                { queued } =
                    midiToNoteList midi
            in
            ( { model
                | playing =
                    Just
                        { elapsed = 0
                        , queued = queued
                        }
              }
            , Cmd.none
            )

        Stop ->
            ( { model | playing = Nothing }, stop () )

        Pick ->
            ( model
            , Select.files [ "audio/midi" ] GotFiles
            )

        DragEnter ->
            ( { model | hover = True }
            , Cmd.none
            )

        DragLeave ->
            ( { model | hover = False }
            , Cmd.none
            )

        GotFiles file files ->
            ( { model | files = Loading, hover = False }
            , List.map File.toBytes (file :: files)
                |> Task.sequence
                |> Task.perform GotBytes
            )

        GotBytes bytes ->
            ( { model | files = Loaded bytes }
            , Cmd.none
            )

        Tick delta ->
            case model.playing of
                Nothing ->
                    ( model, Cmd.none )

                Just ({ elapsed, queued } as playing) ->
                    let
                        elapsed_ =
                            elapsed + delta

                        ( toPlay, toQueue ) =
                            List.break (\{ time } -> toFloat time > elapsed_) queued
                    in
                    ( { model
                        | playing =
                            Just { playing | elapsed = elapsed_, queued = toQueue }
                      }
                    , Cmd.batch <|
                        List.map
                            (\{ key, on } ->
                                play
                                    { freq = keyToFreq key
                                    , on = on
                                    }
                            )
                            toPlay
                    )


keyToFreq key =
    let
        a440 =
            0x3C + 9

        delta =
            toFloat key - a440
    in
    440.0 * 2 ^ (delta / 12)


midiToNoteList : Midi.File -> { queued : List { time : Int, key : Int, on : Bool } }
midiToNoteList midi =
    let
        trackToNoteList track =
            track
                |> List.foldl
                    (\{ deltaTime, event } ( time, acc ) ->
                        ( time + deltaTime
                        , case event of
                            MidiEvent midiEvent ->
                                case midiEvent of
                                    NoteOn { key } ->
                                        { time = time + deltaTime
                                        , key = key
                                        , on = True
                                        }
                                            :: acc

                                    NoteOff { key } ->
                                        { time = time + deltaTime
                                        , key = key
                                        , on = False
                                        }
                                            :: acc

                                    _ ->
                                        acc

                            _ ->
                                acc
                        )
                    )
                    ( 0, [] )
                |> (\( _, notes ) -> List.reverse notes)
    in
    { queued = List.concatMap trackToNoteList midi.tracks }


view : Model -> Html Msg
view model =
    Html.div
        [ hijackOn "dragenter" (D.succeed DragEnter)
        , hijackOn "dragover" (D.succeed DragEnter)
        , hijackOn "dragleave" (D.succeed DragLeave)
        , hijackOn "drop" dropDecoder
        ]
        [ Html.text "Drop a midi file anywhere on the page!"
        , Html.button [ Html.onClick Pick ] [ Html.text "Or click here" ]
        , Html.br [] []
        , case model.files of
            Waiting ->
                Html.text "Waiting for a midi file ^_^"

            Loading ->
                Html.text "Loading..."

            Loaded bytes ->
                Html.div [] <| List.map (viewFile model.playing) bytes
        ]


dropDecoder : D.Decoder Msg
dropDecoder =
    D.at [ "dataTransfer", "files" ] (D.oneOrMore GotFiles File.decoder)


hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
    Html.preventDefaultOn event (D.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )


viewFile : Maybe a -> Bytes -> Html Msg
viewFile playing file =
    case Decode.decode Midi.decoder file of
        Just midi ->
            viewMidi playing midi

        Nothing ->
            Html.text "Invalid midi file =("


viewMidi : Maybe a -> Midi.File -> Html Msg
viewMidi playing ({ header, tracks } as file) =
    let
        row l r =
            Html.tr []
                [ Html.th [] [ Html.text l ]
                , Html.td [] [ Html.text r ]
                ]
    in
    Html.div
        []
    <|
        [ Html.h1 [] [ Html.text "Controls" ]
        , case playing of
            Just _ ->
                Html.button
                    [ Html.onClick <| Stop ]
                    [ Html.text "Stop" ]

            Nothing ->
                Html.button
                    [ Html.onClick <| Play file ]
                    [ Html.text "Play" ]
        , Html.h1 [] [ Html.text "Header" ]
        , Html.table []
            [ row "Format" <| Midi.formatToString header.format
            , row "Tracks" <| String.fromInt header.tracks
            , row "Division" <| Midi.divisionToString header.division
            ]
        , Html.h1 [] [ Html.text "Tracks" ]
        ]
            ++ List.concat
                (List.indexedMap
                    (\i t ->
                        [ Html.h2 [] [ Html.text <| "Track " ++ String.fromInt i ]
                        , viewTrack t
                        ]
                    )
                    tracks
                )


viewTrack : Midi.Track -> Html msg
viewTrack events =
    events
        |> List.map viewEvent
        |> Html.ul []


viewEvent : { deltaTime : Int, event : Midi.Event } -> Html msg
viewEvent { deltaTime, event } =
    Html.li []
        [ Html.text <| String.fromInt deltaTime ++ ": "
        , Midi.viewEvent event
        ]


subscriptions : Model -> Sub Msg
subscriptions { playing } =
    case playing of
        Nothing ->
            Sub.none

        Just _ ->
            Browser.onAnimationFrameDelta Tick


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
