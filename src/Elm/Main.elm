module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HTML exposing (..)
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)
import BST exposing (..)


type Msg
    = NoOp


type alias Model =
    { tree : Tree Int
    }


main : Program Never Model Msg
main =
    program
        { init = Model empty ! []
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


view : Model -> Html Msg
view model =
    svg
        [ Svg.width "640"
        , Svg.height "640"
        , Svg.viewBox "0 0 640 640"
        ]
        [ drawTree model.tree ]


drawTree : Tree comparable -> Svg Msg
drawTree tree =
    let
        depth =
            BST.depth tree
    in
        Svg.text ""


drawNode : Int -> Tree comparable -> Svg Msg
drawNode y node =
    case node of
        Leaf ->
            Svg.text ""

        Node left x right ->
            Svg.text ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


numList : List Int
numList =
    [ 60, 88, 6, 72, 92, 56, 90, 19, 19, 51, 74, 75, 77, 2, 17, 66, 86, 3, 39, 41, 16, 40, 16, 14, 12, 14, 41, 29, 15, 79, 35, 89, 35, 47, 56, 82, 24, 74, 0, 57, 36, 29, 71, 17, 83, 21, 97, 61, 99, 45, 48, 14, 46, 35, 16, 28, 89, 62, 1, 91, 3, 50, 40, 29, 61, 41, 94, 66, 86, 99, 78, 86, 88, 47, 97, 81, 37, 34, 84, 81, 80, 84, 66, 61, 20, 99, 11, 11, 3, 12, 13, 43, 75, 84, 29, 54, 73, 26, 80, 39 ]
