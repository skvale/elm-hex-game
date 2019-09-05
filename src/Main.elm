module Main exposing (..)

import HexView exposing (viewHexagons)
import Json.Decode
import Html exposing (Html)
import Browser exposing (element)
import Html.Attributes exposing (..)
import Model exposing (..)
import Stats exposing (viewStats)
import TurnDetails exposing (viewTurnDetails)


view : Model -> Html Msg
view model =
    Html.div [ class "app" ]
        [ viewStats model
        , viewHexagons model
        , viewTurnDetails model
        ]


main : Program Json.Decode.Value Model Msg
main =
    element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    []
        |> Sub.batch
