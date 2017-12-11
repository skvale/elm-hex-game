module TurnDetails exposing (viewTurnDetails)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)


viewTurnDetails : Model -> Html Msg
viewTurnDetails model =
    div [ class "turn-details" ]
        ([ text "Turn Details" ] ++ (List.map (dog model) (getAnimals model)))


dog : Model -> Animal -> Html Msg
dog model c =
    let
        className =
            if c.key == model.activeAnimal then
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
                , div [] [ text <| "Moved: " ++ toString c.moved ]
                ]
            ]
