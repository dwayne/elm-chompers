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
        , describe "chompOneOrMore"
            [ test "0" <|
                \_ ->
                    P.run natural "0"
                        |> Expect.equal (Ok 0)
            , test "1" <|
                \_ ->
                    P.run natural "1"
                        |> Expect.equal (Ok 1)
            , test "1234567890" <|
                \_ ->
                    P.run natural "1234567890"
                        |> Expect.equal (Ok 1234567890)
            , test "empty" <|
                \_ ->
                    P.run natural ""
                        |> expectDeadEnd P.UnexpectedChar
            , test "large natural" <|
                \_ ->
                    P.run natural "2147483648" -- = maxWWellDefinedInt + 1 == 2^31
                        |> expectDeadEnd (P.Problem "too large")
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


-- EXAMPLE: natural


natural : Parser Int
natural =
    --
    -- natural ::= [0-9]+
    --
    C.chompOneOrMore Char.isDigit
        |> P.getChompedString
        |> P.andThen
            (\s ->
                case String.toInt s of
                    Just n ->
                        if n <= maxWellDefinedInt then
                            P.succeed n

                        else
                            P.problem <| "natural number is too large"

                    Nothing ->
                        P.problem <| "cannot be converted to Int: " ++ s
            )


maxWellDefinedInt : Int
maxWellDefinedInt =
    2 ^ 31 - 1 -- = 2147483647


-- CUSTOM EXPECTATIONS


expectDeadEnd : P.Problem -> Result (List P.DeadEnd) a -> Expect.Expectation
expectDeadEnd expectedProblem result =
    case result of
        Ok _ ->
            -- TODO: Use a better failure message.
            Expect.fail "Fail 1"

        Err deadEnds ->
            let
                expectedDeadEnds =
                    deadEnds
                        |> List.filter
                            (\{ problem } ->
                                case (problem, expectedProblem) of
                                    (P.Problem actual, P.Problem expected) ->
                                        actual |> String.contains expected

                                    -- N.B. This can be extended to do substring equality on:
                                    -- - Expecting String
                                    -- - ExpectingSymbol String
                                    -- - ExpectingKeyword String

                                    _ ->
                                        problem == expectedProblem
                            )
            in
            if List.isEmpty expectedDeadEnds then
                -- TODO: Use a better failure message.
                Expect.fail "Fail 2"

            else
                Expect.pass
