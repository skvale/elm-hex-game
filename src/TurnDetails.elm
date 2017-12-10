module TurnDetails exposing (viewTurnDetails)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)


viewTurnDetails : Model -> Html Msg
viewTurnDetails model =
    div [ class "turn-details" ]
        ([ text "Turn Details" ] ++ (List.map (character model) (getCharacters model)))

character : Model -> Character -> Html Msg
character model c =
    let
        className =
            if c.key == model.activeCharacter then
                " turn-details-active"
            else
                ""
    in
        div [ class <| "turn-details-character" ++ className
            , onClick <| ClickCharacter c]
            [ img [src c.imageHref] []
            , div []
                [ div [] [text c.key]
                , div [] [text <| "Moved: " ++ toString c.moved]
                ]
            ]