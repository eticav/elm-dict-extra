module Main exposing (..)

import Test exposing (..)
import Test.Runner.Html

import ExtDictTests

main : Test.Runner.Html.TestProgram
main =
  [ ExtDictTests.tests
  ]
     |> concat
     |> Test.Runner.Html.run
        

