module View exposing (..)

import Html exposing (..)
import Html.Attributes as Html exposing (..)
import Html.Events as Html exposing (..)
import Svg exposing (..)
import Svg.Attributes as Svg exposing (..)
import Svg.Events as Svg exposing (..)
import BST exposing (..)
import Model exposing (..)
import Json.Decode as Json


view : Model -> Html Msg
view model =
    div []
        [ select
            [ onChange ChangeType ]
            [ option [ value "0" ] [ Html.text "Number" ]
            , option [ value "1" ] [ Html.text "String" ]
            ]
        , input
            [ onInput Input
            , onEnter Add
            , Html.type_ "text"
            , model.input
                |> Maybe.withDefault ""
                |> value
            ]
            []
        , div [ Html.style [ ( "margin", "10px" ) ] ]
            [ case model.type_ of
                Number -> numTreeView model.numTree
                String -> strTreeView model.strTree
            ]
        ]


numTreeView : Tree Float -> Svg Msg
numTreeView tree =
    svg
        [ Svg.width <| toString width
        , Svg.height <| toString height
        , [ 0, 0, width, height ] |> List.map toString |> String.join " " |> Svg.viewBox
        , Html.style [ ( "border", "1px solid black" ) ]
        ]
        [ drawTree Number tree
        ]


strTreeView : Tree String -> Svg Msg
strTreeView tree =
    svg
        [ Svg.width <| toString width
        , Svg.height <| toString height
        , [ 0, 0, width, height ] |> List.map toString |> String.join " " |> Svg.viewBox
        , Html.style [ ( "border", "1px solid black" ) ]
        ]
        [ drawTree String tree
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


drawTree : Type -> Tree comparable -> Svg Msg
drawTree t tree =
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
            |> List.map (draw t radius)
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


draw : Type -> Float -> Drawable comparable -> Svg Msg
draw t radius { x, y, val, left, right } =
    let
        drawLine p =
            line
                [ Svg.x1 <| toString x
                , Svg.y1 <| toString y
                , Svg.x2 <| toString p.x
                , Svg.y2 <| toString p.y
                , Svg.stroke "#2196F3"
                ]
                []
    in
        g []
            [ circle
                [ Svg.cx <| toString x
                , Svg.cy <| toString y
                , Svg.r <| toString radius
                , Svg.fill "#2196F3"
                , Svg.onClick (Delete <| toString val)
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


onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not Enter"
    in
        Html.on "keydown" (Json.andThen isEnter keyCode)


onChange : (String -> msg) -> Html.Attribute msg
onChange msg =
    Html.on "change" (Json.map msg targetValue)
