module Main exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner as BR
import ChompAtLeast
import ChompAtMost
import ChompExactly
import Parser as P


main : BR.BenchmarkProgram
main =
    BR.program benchmarks


benchmarks : Benchmark
benchmarks =
    let
        n =
            1000

        nZeros =
            String.fromList <| List.repeat n '0'
    in
    describe "Benchmarks"
        [ Benchmark.compare "chompExactly"
            "version1"
            (\_ -> P.run (ChompExactly.version1 n Char.isDigit) nZeros)
            "version2"
            -- version2 is faster
            (\_ -> P.run (ChompExactly.version2 n Char.isDigit) nZeros)
        , Benchmark.compare "chompAtLeast"
            "version1"
            -- version 1 is faster
            (\_ -> P.run (ChompAtLeast.version1 n Char.isDigit) (nZeros ++ nZeros))
            "version2"
            (\_ -> P.run (ChompAtLeast.version2 n Char.isDigit) (nZeros ++ nZeros))
        , Benchmark.compare "chompAtMost"
            "version1"
            (\_ -> P.run (ChompAtMost.version1 n Char.isDigit) (nZeros ++ nZeros))
            "version2"
            -- version 2 is faster
            (\_ -> P.run (ChompAtMost.version2 n Char.isDigit) (nZeros ++ nZeros))
        ]
