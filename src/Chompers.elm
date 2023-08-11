module Chompers exposing
    ( chompExactly
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
    if n <= 0 then
        P.succeed ()

    else
        -- if n > 0 then
        P.succeed ()
            |. P.chompIf isGood
            |. chompExactly (n - 1) isGood
