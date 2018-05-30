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
        , branchNumTest
        ]


maxMinTest : Test
maxMinTest =
    describe "maxMinTest"
        [ "maxTest" =>
            (BST.maximum <| BST.fromList [5, 2, 3, 4, 10]) === Just 10
        , "minTest" =>
            (BST.minimum <| BST.fromList [5, 2, 3, 4, 10]) === Just 2
        ]


branchNumTest : Test
branchNumTest =
    describe "branchNumTest"
        [ "right" =>
            (BST.rightNum <| BST.fromList [5, 4, 6, 3, 8]) === 2
        , "left" =>
            (BST.leftNum <| BST.fromList [5, 4, 6, 3, 8]) === 2
        , "rightOnly" =>
            (BST.rightNum <| BST.fromList [1, 2, 3, 4, 5]) === 4
        , "leftOnly" =>
            (BST.leftNum <| BST.fromList [5, 4, 3, 2, 1]) === 4
        ]