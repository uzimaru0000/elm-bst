module Update exposing (..)

import BST exposing (..)
import Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.type_ of
        Number ->
            numUpdate msg model
        String ->
            strUpdate msg model

numUpdate : Msg -> Model -> (Model, Cmd Msg)
numUpdate msg model =
    case msg of
        Input str ->
            { model | input = Just str } ! []
        
        Add ->
            let
                val = model.input
                        |> Maybe.andThen (String.toFloat >> Result.toMaybe)
            in
                case val of
                    Just v ->
                        { model | numTree = insert v model.numTree, input = Nothing } ! []
                    _ ->
                        model ! []
            
        Delete v ->
            let
                val = String.toFloat v
            in
                case val of
                    Ok v ->
                        { model | numTree = delete v model.numTree } ! []
                    _ ->
                        model ! []

        ChangeType str ->
            case String.toInt str of
                Ok n ->
                    { model | type_ = intToType n } ! []
                _ ->
                    model ! []

strUpdate : Msg -> Model -> (Model, Cmd Msg)
strUpdate msg model =
    case msg of
        Input str ->
            { model | input = Just str } ! []
        
        Add ->
            case model.input of
                Just v ->
                    { model | strTree = insert v model.strTree, input = Nothing } ! []
                Nothing ->
                    model ! []
            
        Delete v ->
            { model | strTree = delete v model.strTree } ! []

        ChangeType str ->
            case String.toInt str of
                Ok n ->
                    { model | type_ = intToType n } ! []
                _ ->
                    model ! []


intToType : Int -> Type
intToType n =
    case n of
        0 ->
            Number
        1 ->
            String
        _ ->
            Number