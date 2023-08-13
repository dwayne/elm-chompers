module Test.Chompers exposing (suite)

import Chompers as C
import Expect
import Parser as P exposing ((|.), (|=), Parser)
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
                    P.run natural "2147483648"
                        -- = maxWellDefinedInt + 1 == 2^31
                        |> expectDeadEnd (P.Problem "too large")
            ]
        , describe "chompExactly"
            [ test "a valid basic ZIP code" <|
                \_ ->
                    P.run zipCode "01234"
                        |> Expect.equal (Ok <| Basic "01234")
            , test "a valid extended ZIP code" <|
                \_ ->
                    P.run zipCode "01234-5678"
                        |> Expect.equal (Ok <| Extended { basic = "01234", geoSegment = "5678" })
            , test "invalid example 1" <|
                \_ ->
                    P.run zipCode "012"
                        |> expectDeadEnd P.UnexpectedChar
            , test "invalid example 2" <|
                \_ ->
                    P.run zipCode "012-5678"
                        |> expectDeadEnd P.UnexpectedChar
            , test "invalid example 3" <|
                \_ ->
                    P.run zipCode "01234-5"
                        |> expectDeadEnd P.UnexpectedChar
            ]
        , let
            atLeast3Letters =
                P.getChompedString <|
                    C.chompAtLeast 3 Char.isAlpha
          in
          describe "chompAtLeast"
            [ test "with 3 letters" <|
                \_ ->
                    P.run atLeast3Letters "aBc"
                        |> Expect.equal (Ok "aBc")
            , test "with 7 letters" <|
                \_ ->
                    P.run atLeast3Letters "abcXYZd"
                        |> Expect.equal (Ok "abcXYZd")
            , test "with 2 letters" <|
                \_ ->
                    P.run atLeast3Letters "ab"
                        |> expectDeadEnd P.UnexpectedChar
            , test "empty" <|
                \_ ->
                    P.run atLeast3Letters ""
                        |> expectDeadEnd P.UnexpectedChar
            ]
        , let
            atMost3Letters =
                P.getChompedString <|
                    C.chompAtMost 3 Char.isAlpha
          in
          describe "chompAtMost"
            [ test "empty" <|
                \_ ->
                    P.run atMost3Letters ""
                        |> Expect.equal (Ok "")
            , test "with 1 letter" <|
                \_ ->
                    P.run atMost3Letters "a"
                        |> Expect.equal (Ok "a")
            , test "with 2 letters" <|
                \_ ->
                    P.run atMost3Letters "aB"
                        |> Expect.equal (Ok "aB")
            , test "with 3 letters" <|
                \_ ->
                    P.run atMost3Letters "aBc"
                        |> Expect.equal (Ok "aBc")
            , test "with 4 letters" <|
                \_ ->
                    P.run atMost3Letters "aBcD"
                        |> Expect.equal (Ok "aBc")
            , test "with 5 letters" <|
                \_ ->
                    P.run atMost3Letters "aBcDe"
                        |> Expect.equal (Ok "aBc")
            , test "with a digit as the third character" <|
                \_ ->
                    P.run atMost3Letters "aB1D"
                        |> Expect.equal (Ok "aB")
            ]
        , let
            between3And5Letters =
                P.getChompedString <|
                    C.chompBetween 3 5 Char.isAlpha
          in
          describe "chompBetween"
            [ test "with 3 letters" <|
                \_ ->
                    P.run between3And5Letters "aBc"
                        |> Expect.equal (Ok "aBc")
            , test "with 4 letters" <|
                \_ ->
                    P.run between3And5Letters "aBcD"
                        |> Expect.equal (Ok "aBcD")
            , test "with 5 letters" <|
                \_ ->
                    P.run between3And5Letters "aBcDe"
                        |> Expect.equal (Ok "aBcDe")
            , test "empty" <|
                \_ ->
                    P.run between3And5Letters ""
                        |> expectDeadEnd P.UnexpectedChar
            , test "with 1 letter" <|
                \_ ->
                    P.run between3And5Letters "a"
                        |> expectDeadEnd P.UnexpectedChar
            , test "with 2 letters" <|
                \_ ->
                    P.run between3And5Letters "aB"
                        |> expectDeadEnd P.UnexpectedChar
            , test "with 6 letters" <|
                \_ ->
                    P.run between3And5Letters "aBcDeF"
                        |> Expect.equal (Ok "aBcDe")
            , test "with a digit as the fourth character" <|
                \_ ->
                    P.run between3And5Letters "aBc1e"
                        |> Expect.equal (Ok "aBc")
            , test "with a digit as the fifth character" <|
                \_ ->
                    P.run between3And5Letters "aBcD1F"
                        |> Expect.equal (Ok "aBcD")
            , describe "wierd ranges" <|
                let
                    between m n =
                        P.getChompedString <|
                            C.chompBetween m n Char.isAlpha

                    letters =
                        "abcdefghi"
                in
                [ test "example 1" <|
                    \_ ->
                        P.run (between -3 -1) letters
                            |> Expect.equal (Ok "")
                , test "example 2" <|
                    \_ ->
                        P.run (between -3 2) letters
                            |> Expect.equal (Ok "ab")
                , test "example 3" <|
                    \_ ->
                        P.run (between -1 -3) letters
                            |> Expect.equal (Ok "")
                , test "example 4" <|
                    \_ ->
                        P.run (between 4 -3) letters
                            |> Expect.equal (Ok "")
                ]
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
    -- = 2147483647
    2 ^ 31 - 1



-- EXAMPLE: zipCode


type
    ZipCode
    -- The basic format which consists of 5 digits.
    = Basic String
      -- The extended format, called ZIP+4, which uses
      -- the basic 5-digit code plus 4 additional digits
      -- to identify a geographic segment.
      --
      -- Learn more: https://en.wikipedia.org/wiki/ZIP_Code
    | Extended
        { basic : String
        , geoSegment : String
        }


zipCode : Parser ZipCode
zipCode =
    --
    --  zipCode    ::= basic ('-' geoSegment)?
    --  basic      ::= digit{5}
    --  geoSegment ::= digit{4}
    --  digit      ::= [0-9]
    --
    P.succeed
        (\basic maybeGeoSegment ->
            case maybeGeoSegment of
                Nothing ->
                    Basic basic

                Just geoSegment ->
                    Extended
                        { basic = basic
                        , geoSegment = geoSegment
                        }
        )
        |= nDigits 5
        |= optional
            (P.succeed identity
                |. P.chompIf ((==) '-')
                |= nDigits 4
            )


nDigits : Int -> Parser String
nDigits n =
    P.getChompedString <|
        C.chompExactly n Char.isDigit


optional : Parser a -> Parser (Maybe a)
optional p =
    -- N.B. This is a useful parser to have but it is not included
    -- in chompers since it doesn't primarily operate at the
    -- character level. It's not for the lexical analysis phase.
    P.oneOf
        [ P.map Just p
        , P.succeed Nothing
        ]



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
                                case ( problem, expectedProblem ) of
                                    ( P.Problem actual, P.Problem expected ) ->
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
