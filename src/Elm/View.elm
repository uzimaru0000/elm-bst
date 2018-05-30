module View exposing (..)

import Html exposing (..)
import Html.Attributes as Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)
import BST exposing (..)
import Model exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ input
            [ onInput Input
            , model.input
                |> Maybe.map toString
                |> Maybe.withDefault ""
                |> value
            ]
            []
        , button [ onClick Add ] [ Html.text "Add" ]
        , div [ Html.style [ ( "margin", "10px" ) ] ]
            [ svg
                [ Svg.width <| toString width
                , Svg.height <| toString height
                , [ 0, 0, width, height ] |> List.map toString |> String.join " " |> Svg.viewBox
                , Html.style [ ( "border", "1px solid black" ) ]
                ]
                [ drawTree model.tree
                ]
            ]
        ]



{- Settings -}


width : Float
width =
    640


height : Float
height =
    640


type alias Drawable comparable =
    { x : Float
    , y : Float
    , val : comparable
    , left : Maybe { x : Float, y : Float }
    , right : Maybe { x : Float, y : Float }
    }


xScale : Tree comparable -> Float -> Float
xScale t n =
    width / (toFloat <| BST.num t) * n


yScale : Tree comparable -> Float -> Float
yScale t n =
    height / (toFloat <| BST.depth t) * n


drawTree : Tree comparable -> Svg Msg
drawTree tree =
    let
        list =
            exchange 0 tree |> Tuple.second

        radius =
            width / (toFloat <| BST.num tree) / 2

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
                    exchange (lx + 1) right

                this =
                    { x = lx
                    , y = toFloat <| BST.depth tree
                    , val = v
                    , left =
                        if left == Leaf then
                            Nothing
                        else
                            Just
                                ({ x = lx - 1 - (toFloat <| BST.rightNum left)
                                 , y = toFloat <| BST.depth left
                                 }
                                )
                    , right =
                        if right == Leaf then
                            Nothing
                        else
                            Just
                                ({ x = lx + 1 + (toFloat <| BST.leftNum right)
                                 , y = toFloat <| BST.depth right
                                 }
                                )
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
            , left
                |> Maybe.map drawLine
                |> Maybe.withDefault (Svg.text "")
            , right
                |> Maybe.map drawLine
                |> Maybe.withDefault (Svg.text "")
            , text_
                [ Svg.x <| toString x
                , Svg.y <| toString <| y + radius / 4
                , Svg.fill "black"
                , Svg.fontSize <| toString <| radius
                , Svg.textAnchor "middle"
                ]
                [ Svg.text <| toString val ]
            ]
