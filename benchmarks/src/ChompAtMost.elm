module ChompAtMost exposing (version1, version2)

import ChompExactly
import Parser as P exposing (Parser)


version1 : Int -> (Char -> Bool) -> Parser ()
version1 n isGood =
    List.range 1 n
        |> List.map (\i -> P.backtrackable <| ChompExactly.version2 i isGood)
        |> (::) (P.succeed ())
        |> List.reverse
        |> P.oneOf


version2 : Int -> (Char -> Bool) -> Parser ()
version2 n isGood =
    P.loop 0
        (\i ->
            if i < n then
                P.oneOf
                    [ P.chompIf isGood
                        |> P.andThen (\_ -> P.succeed (P.Loop (i + 1)))
                    , P.succeed <| P.Done ()
                    ]

            else
                P.succeed <| P.Done ()
        )
