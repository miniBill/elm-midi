module Bytes.Decode.Extra exposing (repeat, untilError)

import Bytes.Decode as Decode exposing (Decoder)


{-| Uses the same decoder until it encounters an error. This unfortunately needs a number `n` of maximum bytes to read.

WARNINGS:

1.  This will copy `n` bytes from the input.
2.  If there are `n` items in the `Bytes` value, this will run the provided `Decoder` O(n log n) times!
3.  If the input is `n` bytes long then the coder will be run at most `n` times. This is ok for non-pathological parsers.

Example:

    parseFile bytes =
        Bytes.Decode.Extra.untilError (Bytes.width bytes)

-}
untilError : Int -> Decoder a -> Decoder (List a)
untilError n decoder =
    Decode.bytes n
        |> Decode.andThen
            (\bytes ->
                let
                    bisect r l h =
                        if l >= h then
                            r

                        else
                            let
                                m =
                                    (l + h + 1) // 2
                            in
                            case Decode.decode (repeat m decoder) bytes of
                                Just d ->
                                    bisect d m h

                                Nothing ->
                                    if m == h then
                                        r

                                    else
                                        bisect r l (m - 1)
                in
                Decode.succeed <| bisect [] 0 n
            )


repeat r decoder =
    Decode.loop ( r, [] ) (repeatStep decoder)
        |> Decode.map List.reverse


repeatStep decoder ( left, xs ) =
    if left <= 0 then
        Decode.succeed (Decode.Done xs)

    else
        Decode.map (\x -> Decode.Loop ( left - 1, x :: xs )) decoder
