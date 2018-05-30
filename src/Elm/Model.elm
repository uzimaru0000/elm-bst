module Model exposing (..)

import BST exposing (..)

type Msg
    = Input String
    | Add


type alias Model =
    { tree : Tree Int
    , input : Maybe Int
    }


init : Model
init =
    { tree = fromList []
    , input = Nothing
    }