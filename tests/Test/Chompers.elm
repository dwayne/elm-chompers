module Test.Chompers exposing (suite)

import Chompers as C
import Expect
import Parser as P exposing (Parser)
import Test exposing (..)


suite : Test
suite =
    describe "Chompers"
        [ describe "chompOptional"
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
        , describe "chompZeroOrMore"
            [ test "empty" <|
                \_ ->
                    P.run zeroOrMoreLetters ""
                        |> Expect.equal (Ok "")
            , test "one letter" <|
                \_ ->
                    P.run zeroOrMoreLetters "a1"
                        |> Expect.equal (Ok "a")
            , test "more letters" <|
                \_ ->
                    P.run zeroOrMoreLetters "abcDEF$"
                        |> Expect.equal (Ok "abcDEF")
            , test "no letters" <|
                \_ ->
                    P.run zeroOrMoreLetters "4%!abcDEF"
                        |> Expect.equal (Ok "")
            ]
        ]



-- EXAMPLE: optionalSign


type Sign
    = Positive
    | Negative


optionalSign : Parser (Maybe Sign)
optionalSign =
    --
    -- optionalSign ::= (+|-)?
    --
    C.chompOptional isSign
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


isSign : Char -> Bool
isSign c =
    c == '+' || c == '-'



-- EXAMPLE: zeroOrMoreLetters


zeroOrMoreLetters : Parser String
zeroOrMoreLetters =
    --
    -- zeroOrMoreLetters ::= [a-zA-Z]*
    --
    P.getChompedString <|
        C.chompZeroOrMore Char.isAlpha
