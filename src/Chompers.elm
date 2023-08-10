module Chompers exposing (chompOptional)

import Parser as P exposing (Parser)


chompOptional : Parser () -> Parser ()
chompOptional p =
    P.oneOf
        [ p
        , P.succeed ()
        ]
