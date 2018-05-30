module Model exposing (..)

import BST exposing (..)

type Msg
    = Input String
    | Add
    | Delete String
    | ChangeType String


type Type
    = Number
    | String


type alias Model =
    { strTree : Tree String
    , numTree : Tree Float
    , input : Maybe String
    , type_ : Type
    }


init : Model
init =
    { strTree = fromList []
    , numTree = fromList []
    , input = Nothing
    , type_ = Number
    }