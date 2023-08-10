module Chompers exposing (chompOneOrMore, chompOptional, chompZeroOrMore)

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
