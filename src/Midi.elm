module Midi exposing (Division(..), Event(..), File, Format(..), Header, Track, decoder, divisionToString, formatToString, viewEvent)

import Bitwise
import Bytes exposing (Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Decode.Extra as Decode
import Html exposing (Html)


type alias File =
    { header : Header
    , tracks : List Track
    }


decoder : Decoder File
decoder =
    headerDecoder
        |> Decode.andThen
            (\header ->
                Decode.repeat header.tracks trackDecoder
                    |> Decode.map
                        (\tracks ->
                            { header = header
                            , tracks = tracks
                            }
                        )
            )


type alias Header =
    { format : Format
    , tracks : Int
    , division : Division
    }


type Format
    = SingleTrack
    | SimultaneousTracks
    | IndependantTracks


formatToString : Format -> String
formatToString format =
    case format of
        SingleTrack ->
            "[0] Single track"

        SimultaneousTracks ->
            "[1] Simultaneous Tracks"

        IndependantTracks ->
            "[2] Independant Tracks"


type Division
    = TicksPerQuarterNote Int
    | Frac Int


divisionToString : Division -> String
divisionToString division =
    case division of
        TicksPerQuarterNote ticks ->
            String.fromInt ticks ++ " ticks per 1/4 note"

        Frac int ->
            "TODO: division is " ++ toHex int


log : String -> a -> a
log =
    --Debug.log
    always identity


headerDecoder : Decoder Header
headerDecoder =
    let
        innerHeaderDecoder =
            Decode.map3 Header
                formatDecoder
                --tracks
                (Decode.unsignedInt16 BE)
                --division
                divisionDecoder
    in
    Decode.map2 (\_ c -> c)
        (expectString "MThd")
        (Decode.unsignedInt32 BE
            |> Decode.andThen
                (\hl ->
                    if hl < 6 then
                        let
                            _ =
                                log "header too short" hl
                        in
                        Decode.fail

                    else
                        Decode.map2 (\h _ -> h)
                            innerHeaderDecoder
                            {- The length of the header chunk is 6-bytes.
                               However, software which reads MIDI files is required to honour the length field,
                               even if it is greater than expected. Any unexpected data must be ignored.
                            -}
                            (Decode.repeat (hl - 6) Decode.unsignedInt8)
                )
        )


formatDecoder : Decoder Format
formatDecoder =
    Decode.unsignedInt16 BE
        |> Decode.andThen
            (\n ->
                case n of
                    0 ->
                        Decode.succeed SingleTrack

                    1 ->
                        Decode.succeed SimultaneousTracks

                    2 ->
                        Decode.succeed IndependantTracks

                    _ ->
                        let
                            _ =
                                log "formatDecoder" n
                        in
                        Decode.fail
            )


divisionDecoder : Decoder Division
divisionDecoder =
    Decode.unsignedInt16 BE
        |> Decode.map
            (\r ->
                if r >= 32768 then
                    Frac (r - 32768)

                else
                    TicksPerQuarterNote r
            )


expectString : String -> Decoder ()
expectString expected =
    Decode.string 4
        |> Decode.andThen
            (\actual ->
                if actual == expected then
                    Decode.succeed ()

                else
                    let
                        _ =
                            log "expectString" { expected = expected, actual = actual }
                    in
                    Decode.fail
            )


type Event
    = MidiEvent MidiEvent
    | SysexEvent SysexEvent
    | MetaEvent MetaEvent
    | UnknownEvent


type MidiEvent
    = NoteOn { channel : Int, key : Int, velocity : Int }
    | NoteOff { channel : Int, key : Int, velocity : Int }
    | ProgramChange { channel : Int, program : Int }
    | AllSoundOff { channel : Int }
    | ControllerChange { channel : Int, controller : Int, value : Int }


type MetaEvent
    = CopyrightNotice { value : String }
    | TrackName { value : String }
    | InstrumentName { value : String }
    | SetTempo { microsecondsPerQuarterNote : Int }
    | TimeSignature
        { numerator : Int
        , denominatorPower : Int
        , midiClocksPerMetronomeTick : Int
        , num32thPer24MidiClocks : Int
        }
    | KeySignature
        { sharpsOrFlats : SharpsOrFlats
        , majorOrMinor : MajorOrMinor
        }
    | EndOfTrack
    | UnknownMetaEvent { type_ : String, data : List String }


type SysexEvent
    = ResetEvent
        { deviceId : DeviceId
        , modelId : String
        }
    | GeneralMidiOnEvent { deviceId : DeviceId }
    | GeneralMidiOffEvent { deviceId : DeviceId }
    | RolandSysexEvent
        { deviceId : DeviceId
        , modelId : String
        , sendingOrRequesting : SendingOrRequesting
        , addr : String
        , data : List String
        , checksum : String
        }
    | UnknownSysexEvent (List String)
    | NonRealTimeSysexEvent
        { deviceId : DeviceId
        , subID1 : String
        , subID2 : String
        , data : List String
        }
    | RealTimeSysexEvent
        { deviceId : DeviceId
        , subID1 : String
        , subID2 : String
        , data : List String
        }


viewEvent : Event -> Html msg
viewEvent event =
    let
        int a b =
            ( a, String.fromInt b )

        string a b =
            ( a, b )

        list a b =
            ( a, String.join " " b )

        view label vars =
            Html.text <| label ++ " " ++ String.join ", " (List.map (\( k, v ) -> k ++ ": " ++ v) vars)
    in
    case event of
        MidiEvent (NoteOn { channel, key, velocity }) ->
            view "Note on" [ int "channel" channel, int "key" key, int "velocity" velocity ]

        MidiEvent (NoteOff { channel, key, velocity }) ->
            view "Note off" [ int "channel" channel, int "key" key, int "velocity" velocity ]

        MidiEvent (AllSoundOff { channel }) ->
            view "All sound off" [ int "channel" channel ]

        MidiEvent (ProgramChange { channel, program }) ->
            view "Program change" [ int "channel" channel, int "program" program ]

        MidiEvent (ControllerChange { channel, controller, value }) ->
            view "Controller change" [ int "channel" channel, int "controller" controller, int "value" value ]

        MetaEvent (CopyrightNotice { value }) ->
            view "CopyrightNotice" [ string "value" value ]

        MetaEvent (TrackName { value }) ->
            view "TrackName" [ string "value" value ]

        MetaEvent (InstrumentName { value }) ->
            view "InstrumentName" [ string "value" value ]

        MetaEvent (SetTempo { microsecondsPerQuarterNote }) ->
            view "SetTempo" [ int "microsecondsPerQuarterNote" microsecondsPerQuarterNote ]

        MetaEvent (TimeSignature { numerator, denominatorPower, midiClocksPerMetronomeTick, num32thPer24MidiClocks }) ->
            view "TimeSignature" [ int "numerator" numerator, int "denominatorPower" denominatorPower, int "midiClocksPerMetronomeTick" midiClocksPerMetronomeTick, int "num32thPer24MidiClocks" num32thPer24MidiClocks ]

        MetaEvent (KeySignature { sharpsOrFlats, majorOrMinor }) ->
            view "KeySignature" [ string "sharpsOrFlats" <| sharpsOrFlatsToString sharpsOrFlats, string "majorOrMinor" <| majorOrMinorToString majorOrMinor ]

        MetaEvent EndOfTrack ->
            view "EndOfTrack" []

        MetaEvent (UnknownMetaEvent { type_, data }) ->
            view "UnknownMetaEvent" [ string "type_" type_, list "data" data ]

        SysexEvent (ResetEvent { deviceId, modelId }) ->
            view "ResetEvent" [ ( "deviceId", deviceIdToString deviceId ), string "modelId" modelId ]

        SysexEvent (GeneralMidiOnEvent { deviceId }) ->
            view "GeneralMidiOnEvent" [ ( "deviceId", deviceIdToString deviceId ) ]

        SysexEvent (GeneralMidiOffEvent { deviceId }) ->
            view "GeneralMidiOffEvent" [ ( "deviceId", deviceIdToString deviceId ) ]

        SysexEvent (RolandSysexEvent { deviceId, modelId, sendingOrRequesting, addr, data, checksum }) ->
            view "RolandSysexEvent" [ ( "deviceId", deviceIdToString deviceId ), string "modelId" modelId, ( "sendingOrRequesting", sendingOrRequestingToString sendingOrRequesting ), string "addr" addr, list "data" data, string "checksum" checksum ]

        SysexEvent (NonRealTimeSysexEvent { deviceId, subID1, subID2, data }) ->
            view "NonRealTimeSysexEvent" [ ( "deviceId", deviceIdToString deviceId ), string "subID1" subID1, string "subID2" subID2, list "data" data ]

        SysexEvent (RealTimeSysexEvent { deviceId, subID1, subID2, data }) ->
            view "RealTimeSysexEvent" [ ( "deviceId", deviceIdToString deviceId ), string "subID1" subID1, string "subID2" subID2, list "data" data ]

        SysexEvent (UnknownSysexEvent data) ->
            view "UnknownSysexEvent" [ list "data" data ]

        UnknownEvent ->
            view "UnknownEvent" []


deviceIdToString : DeviceId -> String
deviceIdToString deviceId =
    case deviceId of
        AllDevices ->
            "[7F] all devices"

        SpecificDevice spec ->
            toHex spec


sharpsOrFlatsToString : SharpsOrFlats -> String
sharpsOrFlatsToString sharpsOrFlats =
    case sharpsOrFlats of
        KeySharps s ->
            String.fromInt s ++ " sharps"

        KeyC ->
            "C Major"

        KeyFlats f ->
            String.fromInt f ++ " flags"


majorOrMinorToString : MajorOrMinor -> String
majorOrMinorToString majorOrMinor =
    case majorOrMinor of
        Major ->
            "Major"

        Minor ->
            "Minor"


sendingOrRequestingToString : SendingOrRequesting -> String
sendingOrRequestingToString sor =
    case sor of
        Sending ->
            "Sending"

        Requesting ->
            "Requesting"


type DeviceId
    = AllDevices
    | SpecificDevice Int


type SendingOrRequesting
    = Sending
    | Requesting


type SharpsOrFlats
    = KeySharps Int
    | KeyC
    | KeyFlats Int


type MajorOrMinor
    = Major
    | Minor


type alias Track =
    List
        { deltaTime : Int
        , event : Event
        }


innerTrackDecoder : Decoder Track
innerTrackDecoder =
    let
        step ( bytesLeft, acc ) =
            let
                _ =
                    log "step" bytesLeft
            in
            if bytesLeft < 0 then
                let
                    _ =
                        log "innerTrackDecoder | bytesLeft" bytesLeft
                in
                Decode.succeed <|
                    --log "fail" <|
                    Done (List.reverse acc)

            else if bytesLeft == 0 then
                Decode.succeed <| Done (List.reverse acc)

            else
                eventDecoder
                    |> Decode.map
                        (\{ deltaTime, event, length } ->
                            Loop
                                ( bytesLeft - length
                                , { deltaTime = deltaTime, event = event } :: acc
                                )
                        )
    in
    Decode.unsignedInt32 BE
        |> Decode.andThen (\l -> Decode.loop ( l, [] ) step)


variableLengthData : Decoder { value : List Int, length : Int }
variableLengthData =
    variableLengthInt
        |> Decode.andThen
            (\length ->
                Decode.map
                    (\value ->
                        { value = value
                        , length = length.length + length.value
                        }
                    )
                    (Decode.repeat length.value Decode.unsignedInt8)
            )


eventDecoder : Decoder { deltaTime : Int, event : Event, length : Int }
eventDecoder =
    let
        innerEventDecoder b =
            let
                _ =
                    log "innerEventDecoder" ( toHex <| Bitwise.shiftRightZfBy 4 b, toHex b )

                n =
                    Bitwise.and 0x0F b
            in
            case ( Bitwise.shiftRightZfBy 4 b, b ) of
                ( 0x08, _ ) ->
                    Decode.map2
                        (\k v ->
                            { value =
                                MidiEvent <|
                                    NoteOff
                                        { channel = n
                                        , key = k
                                        , velocity = v
                                        }
                            , length = 2
                            }
                        )
                        Decode.unsignedInt8
                        Decode.unsignedInt8

                ( 0x09, _ ) ->
                    Decode.map2
                        (\k v ->
                            { value =
                                MidiEvent <|
                                    NoteOn
                                        { channel = n
                                        , key = k
                                        , velocity = v
                                        }
                            , length = 2
                            }
                        )
                        Decode.unsignedInt8
                        Decode.unsignedInt8

                ( 0x0C, _ ) ->
                    Decode.map
                        (\p ->
                            { value = MidiEvent <| ProgramChange { channel = n, program = p }
                            , length = 1
                            }
                        )
                        Decode.unsignedInt8

                ( _, 0xF0 ) ->
                    sysexEventDecoder { prependF0 = True }

                ( _, 0xF7 ) ->
                    sysexEventDecoder { prependF0 = False }

                ( _, 0xFF ) ->
                    metaEventDecoder

                ( 0x0B, _ ) ->
                    bEventDecoder n

                _ ->
                    let
                        _ =
                            log "UnknownEvent" ( toHex <| Bitwise.shiftRightZfBy 4 b, toHex b )
                    in
                    Decode.succeed
                        { value = UnknownEvent
                        , length = 0
                        }
    in
    Decode.map2
        (\deltaTime event ->
            log "eventDecoder"
                { deltaTime = deltaTime.value
                , event = event.value
                , length = deltaTime.length + 1 + event.length
                }
        )
        variableLengthInt
        (Decode.unsignedInt8
            |> Decode.andThen innerEventDecoder
        )


bEventDecoder : Int -> Decoder { value : Event, length : Int }
bEventDecoder n =
    Decode.map2
        (\cc nn ->
            { value =
                case ( cc, nn ) of
                    ( 0x78, 0 ) ->
                        MidiEvent <| AllSoundOff { channel = n }

                    _ ->
                        if cc < 0x77 then
                            MidiEvent <| ControllerChange { channel = n, controller = cc, value = nn }

                        else
                            let
                                _ =
                                    log "UnknownBEvent" ( n, cc, nn )
                            in
                            UnknownEvent
            , length = 2
            }
        )
        Decode.unsignedInt8
        Decode.unsignedInt8


metaEventDecoder : Decoder { value : Event, length : Int }
metaEventDecoder =
    Decode.map2
        (\type_ data ->
            log "metaEventDecoder" <|
                { value =
                    MetaEvent <|
                        case ( type_, data.value ) of
                            ( 0x02, _ ) ->
                                CopyrightNotice { value = String.fromList <| List.map Char.fromCode data.value }

                            ( 0x03, _ ) ->
                                TrackName { value = String.fromList <| List.map Char.fromCode data.value }

                            ( 0x04, _ ) ->
                                InstrumentName { value = String.fromList <| List.map Char.fromCode data.value }

                            ( 0x51, [ t1, t2, t3 ] ) ->
                                SetTempo
                                    { microsecondsPerQuarterNote = t1 * 256 * 256 + t2 * 256 + t3
                                    }

                            ( 0x58, [ nn, dd, cc, bb ] ) ->
                                TimeSignature
                                    { numerator = nn
                                    , denominatorPower = dd
                                    , midiClocksPerMetronomeTick = cc
                                    , num32thPer24MidiClocks = bb
                                    }

                            ( 0x59, [ sf, mi ] ) ->
                                KeySignature
                                    { sharpsOrFlats =
                                        if sf > 0 then
                                            KeySharps sf

                                        else if sf == 0 then
                                            KeyC

                                        else
                                            KeyFlats -sf
                                    , majorOrMinor =
                                        if mi == 0 then
                                            Major

                                        else
                                            Minor
                                    }

                            ( 0x2F, [] ) ->
                                EndOfTrack

                            _ ->
                                UnknownMetaEvent
                                    { type_ = toHex type_
                                    , data = List.map toHex data.value
                                    }
                , length = data.length + 1
                }
        )
        Decode.unsignedInt8
        variableLengthData


dropRight : Int -> List a -> List a
dropRight n =
    List.reverse >> List.drop n >> List.reverse


sysexEventDecoder : { prependF0 : Bool } -> Decoder { value : Event, length : Int }
sysexEventDecoder { prependF0 } =
    Decode.map
        (\data ->
            { value =
                SysexEvent <|
                    let
                        default _ =
                            let
                                _ =
                                    log "UnknownSysexEvent" <| List.map toHex data.value

                                finalData =
                                    List.map toHex <|
                                        if prependF0 then
                                            0xF0 :: data.value

                                        else
                                            data.value
                            in
                            UnknownSysexEvent finalData
                    in
                    case data.value of
                        [ 0x41, deviceId, modelId, 0x12, 0x40, 0x00, 0x7F, 0x00, _, 0xF7 ] ->
                            ResetEvent
                                { deviceId = deviceIdDecoder deviceId
                                , modelId = toHex modelId
                                }

                        0x41 :: deviceId :: modelId :: sendingOrRequesting :: addr1 :: addr2 :: addr3 :: rest ->
                            case List.reverse rest of
                                0xF7 :: checksum :: sentData ->
                                    RolandSysexEvent
                                        { deviceId = deviceIdDecoder deviceId
                                        , modelId = toHex modelId
                                        , sendingOrRequesting =
                                            if sendingOrRequesting == 0x12 then
                                                Sending

                                            else
                                                Requesting
                                        , addr = toHex <| addr1 * 256 * 256 + addr2 * 256 + addr3
                                        , data =
                                            sentData
                                                |> List.reverse
                                                |> List.map toHex
                                        , checksum = toHex checksum
                                        }

                                _ ->
                                    default ()

                        [ 0x7E, deviceId, 0x09, 0x01, 0xF7 ] ->
                            GeneralMidiOnEvent
                                { deviceId = deviceIdDecoder deviceId
                                }

                        [ 0x7E, deviceId, 0x09, 0x02, 0xF7 ] ->
                            GeneralMidiOffEvent
                                { deviceId = deviceIdDecoder deviceId
                                }

                        0x7E :: deviceId :: subID1 :: subID2 :: rest ->
                            case List.reverse rest of
                                0xF7 :: sentData ->
                                    NonRealTimeSysexEvent
                                        { deviceId = deviceIdDecoder deviceId
                                        , subID1 = toHex subID1
                                        , subID2 = toHex subID2
                                        , data =
                                            sentData
                                                |> List.reverse
                                                |> List.map toHex
                                        }

                                _ ->
                                    default ()

                        0x7F :: deviceId :: subID1 :: subID2 :: rest ->
                            case List.reverse rest of
                                0xF7 :: sentData ->
                                    RealTimeSysexEvent
                                        { deviceId = deviceIdDecoder deviceId
                                        , subID1 = toHex subID1
                                        , subID2 = toHex subID2
                                        , data =
                                            sentData
                                                |> List.reverse
                                                |> List.map toHex
                                        }

                                _ ->
                                    default ()

                        _ ->
                            default ()
            , length = data.length
            }
        )
        variableLengthData


deviceIdDecoder : Int -> DeviceId
deviceIdDecoder i =
    if i == 0x7F then
        AllDevices

    else
        SpecificDevice i


toHex : Int -> String
toHex =
    let
        toHexDigit d =
            case d of
                0x0A ->
                    'A'

                0x0B ->
                    'B'

                0x0C ->
                    'C'

                0x0D ->
                    'D'

                0x0E ->
                    'E'

                0x0F ->
                    'F'

                _ ->
                    Char.fromCode <| (d + Char.toCode '0')

        go acc x =
            if x == 0 then
                String.fromList acc

            else
                go (toHexDigit (modBy 16 x) :: acc) (x // 16)
    in
    go [] >> String.padLeft 2 '0'


variableLengthInt : Decoder { value : Int, length : Int }
variableLengthInt =
    let
        step ( acc, len ) =
            Decode.unsignedInt8
                |> Decode.map
                    (\byte ->
                        if byte >= 128 then
                            Loop ( acc * 128 + byte - 128, len + 1 )

                        else
                            Done { value = acc * 128 + byte, length = len + 1 }
                    )
    in
    Decode.loop ( 0, 0 ) step


trackDecoder : Decoder Track
trackDecoder =
    Decode.repeat 4 Decode.unsignedInt8
        |> Decode.andThen
            (\r ->
                let
                    t =
                        String.fromList <| List.map Char.fromCode r
                in
                case t of
                    "MTrk" ->
                        innerTrackDecoder

                    _ ->
                        --Spec requires ignoring unexpected chunks
                        Decode.map2 (\_ c -> c)
                            unknownChunkDecoder
                            trackDecoder
            )


unknownChunkDecoder : Decoder ()
unknownChunkDecoder =
    Decode.unsignedInt32 BE
        |> Decode.andThen (\l -> Decode.repeat l Decode.unsignedInt8)
        |> Decode.map (\_ -> ())
