module ChompExactly exposing (version1, version2)

import Parser as P exposing ((|.), (|=), Parser)


version1 : Int -> (Char -> Bool) -> Parser ()
version1 n isGood =
    if n <= 0 then
        P.succeed ()

    else
        -- if n > 0 then
        P.succeed ()
            |. P.chompIf isGood
            |. version1 (n - 1) isGood


version2 : Int -> (Char -> Bool) -> Parser ()
version2 n isGood =
    P.loop n
        (\i ->
            if i <= 0 then
                P.succeed <| P.Done ()

            else
                P.chompIf isGood
                    |> P.andThen (\_ -> P.succeed (P.Loop (i - 1)))
        )
