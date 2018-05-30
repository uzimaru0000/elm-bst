module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Html exposing (..)
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)
import BST exposing (..)


type Msg
    = NoOp


type alias Model =
    { tree : Tree Int
    }


type alias Drawable comparable =
    { x : Float
    , y : Float
    , val : comparable
    , left : Maybe { x : Float, y : Float }
    , right : Maybe { x : Float, y : Float }
    }


main : Program Never Model Msg
main =
    program
        { init = Model (BST.fromList numList) ! []
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
        , Html.style [ ( "border", "1px solid black" ) ]
        ]
        [ drawTree model.tree
        ]


width : Float
width =
    640


height : Float
height =
    640


xScale : Tree comparable -> Float -> Float
xScale t n =
    width / (toFloat <| BST.length t) * n


yScale : Tree comparable -> Float -> Float
yScale t n =
    height / (toFloat <| BST.depth t) * n


drawTree : Tree comparable -> Svg Msg
drawTree tree =
    let
        list =
            exchange 0 tree |> Tuple.second

        radius =
            width / (toFloat <| BST.length tree) / 2

        depth =
            toFloat <| BST.depth tree

        scaler { x, y } =
            let
                xScaler =
                    xScale tree

                yScaler =
                    yScale tree
            in
                { x = xScaler x + radius, y = (+) radius <| yScaler <| depth - y }
    in
        list
            |> List.map
                (\({ left, right } as v) ->
                    let
                        newLeft =
                            Maybe.map scaler left

                        newRight =
                            Maybe.map scaler right

                        newPos =
                            scaler v
                    in
                        { v
                            | x = newPos.x
                            , y = newPos.y
                            , left = newLeft
                            , right = newRight
                        }
                )
            |> List.map (draw radius)
            |> g []


exchange : Float -> Tree comparable -> ( Float, List (Drawable comparable) )
exchange x tree =
    case tree of
        Leaf ->
            ( x, [] )

        Node left v right ->
            let
                ( lx, lli ) =
                    exchange x left

                ( rx, rli ) =
                    exchange (lx + 1.0) right

                this =
                    { x = lx
                    , y = toFloat <| BST.depth tree
                    , val = v
                    , left =
                        if left == Leaf then
                            Nothing
                        else
                            Just ({ x = lx - (toFloat <| BST.rightNum left), y = toFloat <| BST.depth left })
                    , right =
                        if right == Leaf then
                            Nothing
                        else
                            Just ({ x = rx - (toFloat <| BST.leftNum right), y = toFloat <| BST.depth right })
                    }
            in
                ( rx, lli ++ [ this ] ++ rli )


draw : Float -> Drawable comparable -> Svg Msg
draw radius { x, y, val, left, right } =
    let
        drawLine p =
            line
                [ Svg.x1 <| toString x
                , Svg.y1 <| toString y
                , Svg.x2 <| toString p.x
                , Svg.y2 <| toString p.y
                , Svg.stroke "blue"
                ]
                []
    in
        g []
            [ circle
                [ Svg.cx <| toString x
                , Svg.cy <| toString y
                , Svg.r <| toString radius
                , Svg.fill "blue"
                ]
                []
            , text_
                [ Svg.x <| toString x
                , Svg.y <| toString y
                , Svg.fill "black"
                , Svg.fontSize <| toString <| radius / 2
                , Svg.textAnchor "middle"
                ]
                [ Svg.text <| toString val ]
            , left
                |> Maybe.map drawLine
                |> Maybe.withDefault (Svg.text "")
            , right
                |> Maybe.map drawLine
                |> Maybe.withDefault (Svg.text "")
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


numList1 : List Int
numList1 =
    [ 5, 4, 3, 6, 8 ]


numList2 : List Int
numList2 =
    [ 5, 2, 8, 0, 1, 3, 4, 6, 7, 9, 10 ]


numList : List Int
numList =
    [ 60, 88, 6, 72, 92, 56, 90, 19, 19, 51, 74, 75, 77, 2, 17, 66, 86, 3, 39, 41, 16, 40, 16, 14, 12, 14, 41, 29, 15, 79, 35, 89, 35, 47, 56, 82, 24, 74, 0, 57, 36, 29, 71, 17, 83, 21, 97, 61, 99, 45, 48, 14, 46, 35, 16, 28, 89, 62, 1, 91, 3, 50, 40, 29, 61, 41, 94, 66, 86, 99, 78, 86, 88, 47, 97, 81, 37, 34, 84, 81, 80, 84, 66, 61, 20, 99, 11, 11, 3, 12, 13, 43, 75, 84, 29, 54, 73, 26, 80, 39 ]
