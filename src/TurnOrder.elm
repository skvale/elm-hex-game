module TurnOrder exposing (viewTurnOrder)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)


viewTurnOrder : model -> Html msg
viewTurnOrder model =
    div [ class "turn-order" ] [ text "Turn Order" ]
