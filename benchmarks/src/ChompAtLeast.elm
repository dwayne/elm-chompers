module ChompAtLeast exposing (version1, version2)

import ChompExactly
import Parser as P exposing ((|.), Parser)


version1 : Int -> (Char -> Bool) -> Parser ()
version1 m isGood =
    P.succeed ()
        |. ChompExactly.version2 m isGood
        |. P.chompWhile isGood


version2 : Int -> (Char -> Bool) -> Parser ()
version2 m isGood =
    P.loop 0
        (\i ->
            let
                next =
                    P.chompIf isGood
                        |> P.andThen (\_ -> P.succeed (P.Loop (i + 1)))
            in
            if i >= m then
                P.oneOf
                    [ next
                    , P.succeed <| P.Done ()
                    ]

            else
                next
        )
