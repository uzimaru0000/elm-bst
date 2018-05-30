module Update exposing (..)

import BST exposing (..)
import Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input str ->
            { model | input = (String.toInt >> Result.toMaybe) str } ! []
        Add ->
            case model.input of
                Just n ->
                    { model | tree = insert n model.tree, input = Nothing } ! []
                Nothing ->
                    model ! []