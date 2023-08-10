module Chompers exposing (chompOptional, chompZeroOrMore)

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
