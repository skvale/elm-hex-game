module Stats exposing (viewStats)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Hexagons.HexContent exposing (..)


viewStats : Model -> Html msg
viewStats model =
    let
        maybeCharacter =
            Dict.get model.activeCharacter model.characters
    in
        case maybeCharacter of
            Just character ->
                div [ class "stats" ]
                    [ header
                    , activePlayer character
                    , health character
                    , experience character
                    , location character
                    , items model.items character
                    ]

            _ ->
                div [ class "stats" ]
                    [ header
                    ]


header : Html msg
header =
    div [ class "stats-header" ]
        [ text "Game stats" ]


activePlayer : Character -> Html msg
activePlayer character =
    let
        display =
            character.key
    in
        div [ class "stats-li stats-active-player" ]
            [ text ("Active: " ++ display)
            , img [ src character.imageHref, class "stats-active-player-img" ] []
            ]


health : Character -> Html msg
health character =
    let
        display =
            (toString character.health) ++ " / " ++ (toString character.totalHealth)
    in
        div [ class "stats-li stats-health" ] [ text ("Health: " ++ display) ]


experience : Character -> Html msg
experience character =
    let
        display =
            (toString character.experience) ++ " / " ++ ("100")
    in
        div [ class "stats-li stats-experience" ] [ text ("Experience: " ++ display) ]


location : Character -> Html msg
location character =
    let
        display =
            toString character.location
    in
        div [ class "stats-li stats-location" ] [ text ("Location: " ++ display) ]


items : Dict.Dict String Item -> Character -> Html msg
items items character =
    let
        display =
            List.map
                (\item ->
                    div [ class "stats-item" ]
                        [ Dict.get item items |> Maybe.map .name |> toString |> text
                        ]
                )
                character.items
    in
        div [ class "stats-li stats-items" ] ([ text "Items: " ] ++ display)
