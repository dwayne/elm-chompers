module Test.Chompers exposing (suite)

import Chompers as C
import Expect
import Parser as P exposing (Parser)
import Test exposing (..)


suite : Test
suite =
    describe "Chompers"
        [ chompOptionalSuite
        ]


chompOptionalSuite : Test
chompOptionalSuite =
    describe "chompOptional"
        [ describe "optionalSign"
            [ test "no sign" <|
                \_ ->
                    P.run optionalSign ""
                        |> Expect.equal (Ok Nothing)
            , test "+ sign" <|
                \_ ->
                    P.run optionalSign "+"
                        |> Expect.equal (Ok <| Just Positive)
            , test "- sign" <|
                \_ ->
                    P.run optionalSign "-"
                        |> Expect.equal (Ok <| Just Negative)
            , test "not a sign" <|
                \_ ->
                    P.run optionalSign "abc"
                        |> Expect.equal (Ok Nothing)
            ]
        ]



-- EXAMPLE: optionalSign


type Sign
    = Positive
    | Negative



--
-- optionalSign ::= (+|-)?
--


optionalSign : Parser (Maybe Sign)
optionalSign =
    chompOptionalSign
        |> P.mapChompedString
            (\s _ ->
                if s == "" then
                    Nothing

                else if s == "+" then
                    Just Positive

                else
                    -- if s == "-" then
                    Just Negative
            )


chompOptionalSign : Parser ()
chompOptionalSign =
    C.chompOptional (P.chompIf isSign)


isSign : Char -> Bool
isSign c =
    c == '+' || c == '-'
