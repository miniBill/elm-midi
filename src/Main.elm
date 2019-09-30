module Main exposing (main)

import Browser
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode
import File exposing (File)
import File.Select as Select
import Html exposing (Attribute, Html, text)
import Html.Events as Html
import Json.Decode as D
import Midi
import Task


type alias Model =
    { hover : Bool
    , files : Files
    }


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


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { hover = False
      , files = Waiting
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                Html.div [] <| List.map viewFile bytes
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


viewFile : Bytes -> Html msg
viewFile file =
    case Decode.decode Midi.decoder file of
        Just midi ->
            viewMidi midi

        Nothing ->
            Html.text "Invalid midi file =("


viewMidi : Midi.File -> Html msg
viewMidi { header, tracks } =
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
        [ Html.h1 [] [ Html.text "Header" ]
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
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
