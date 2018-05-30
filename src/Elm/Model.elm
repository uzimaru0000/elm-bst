module Model exposing (..)

import BST exposing (..)

type Msg
    = Input String
    | Add
    | Delete String


type alias Model =
    { tree : Tree String
    , input : Maybe String
    }


init : Model
init =
    { tree = fromList []
    , input = Nothing
    }