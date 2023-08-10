module Chompers exposing (chompOptional)

import Parser as P exposing (Parser)


chompOptional : (Char -> Bool) -> Parser ()
chompOptional isGood =
    P.oneOf
        [ P.chompIf isGood
        , P.succeed ()
        ]
