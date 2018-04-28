module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TestExtend exposing (..)
import BST as BST


all : Test
all =
    describe "allTest"
        [ maxMinTest
        ]


maxMinTest : Test
maxMinTest =
    describe "maxMinTest"
        [ "maxTest" =>
            (BST.maximum <| BST.fromList [5, 2, 3, 4, 10]) === Just 10
        , "minTest" =>
            (BST.minimum <| BST.fromList [5, 2, 3, 4, 10]) === Just 2
        ]