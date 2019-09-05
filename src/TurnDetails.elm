module TurnDetails exposing (viewTurnDetails)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)


viewTurnDetails : Model -> Html Msg
viewTurnDetails model =
    div [ class "turn-details" ]
        [ text "Turn Details"
        , div [] (List.map (dog model) (Dict.values model.dogs))
        , button [ class "turn-details-end-turn", onClick TurnEnd ] [ text "End turn" ]
        ]


dog : Model -> Animal -> Html Msg
dog model c =
    let
        className =
            if c.key == model.activeDog then
                " turn-details-active"

            else
                ""
    in
    div
        [ class <| "turn-details-dog" ++ className
        , onClick <| ClickDog c
        ]
        [ img [ src c.imageHref ] []
        , div []
            [ div [] [ text c.key ]
            , div [] [ text <| "Moved: " ++ String.fromInt c.moved ]
            ]
        ]
