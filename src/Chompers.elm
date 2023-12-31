module Chompers exposing
    ( chompAtLeast
    , chompAtMost
    , chompBetween
    , chompExactly
    , chompOneOrMore
    , chompOptional
    , chompZeroOrMore
    )

import Parser as P exposing ((|.), Parser)


chompOptional : (Char -> Bool) -> Parser ()
chompOptional isGood =
    P.oneOf
        [ P.chompIf isGood
        , P.succeed ()
        ]


chompZeroOrMore : (Char -> Bool) -> Parser ()
chompZeroOrMore =
    P.chompWhile


chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore isGood =
    P.succeed ()
        |. P.chompIf isGood
        |. P.chompWhile isGood


chompExactly : Int -> (Char -> Bool) -> Parser ()
chompExactly n isGood =
    P.loop n
        (\i ->
            if i <= 0 then
                P.succeed <| P.Done ()

            else
                P.chompIf isGood
                    |> P.andThen (\_ -> P.succeed (P.Loop (i - 1)))
        )


chompAtLeast : Int -> (Char -> Bool) -> Parser ()
chompAtLeast m isGood =
    P.succeed ()
        |. chompExactly m isGood
        |. P.chompWhile isGood


chompAtMost : Int -> (Char -> Bool) -> Parser ()
chompAtMost n isGood =
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


chompBetween : Int -> Int -> (Char -> Bool) -> Parser ()
chompBetween m n isGood =
    if m > n then
        P.succeed ()

    else
        let
            l =
                max m 0

            h =
                max l n
        in
        chompExactly l isGood
            |> P.andThen (\_ -> chompAtMost (h - l) isGood)
