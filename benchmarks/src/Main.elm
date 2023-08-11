module Main exposing (main)

import Benchmark exposing (Benchmark, describe)
import Benchmark.Runner as BR
import ChompExactly
import Parser as P


main : BR.BenchmarkProgram
main =
    BR.program benchmarks


benchmarks : Benchmark
benchmarks =
    describe "Benchmarks"
        [ let
            n =
                1000

            nZeros =
                String.fromList <| List.repeat n '0'
          in
          Benchmark.compare "chompExactly"
            "version1"
            (\_ -> P.run (ChompExactly.version1 n Char.isDigit) nZeros)
            "version2"
            (\_ -> P.run (ChompExactly.version2 n Char.isDigit) nZeros)
        ]
