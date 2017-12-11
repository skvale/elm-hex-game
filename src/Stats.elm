module Stats exposing (viewStats)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import Hexagons.HexContent exposing (..)


viewStats : Model -> Html msg
viewStats model =
    let
        maybeAnimal =
            Dict.get model.activeAnimal model.dogs
    in
        case maybeAnimal of
            Just dog ->
                div [ class "stats" ]
                    [ header
                    , activePlayer dog
                    , health dog
                    , experience dog
                    , location dog
                    , items model.items dog
                    ]

            _ ->
                div [ class "stats" ]
                    [ header
                    ]


header : Html msg
header =
    div [ class "stats-header" ]
        [ text "Game stats" ]


activePlayer : Animal -> Html msg
activePlayer dog =
    let
        display =
            dog.key
    in
        div [ class "stats-li stats-active-player" ]
            [ text ("Active: " ++ display)
            , img [ src dog.imageHref, class "stats-active-player-img" ] []
            ]


health : Animal -> Html msg
health dog =
    let
        display =
            (toString dog.health) ++ " / " ++ (toString dog.totalHealth)
    in
        div [ class "stats-li stats-health" ] [ text ("Health: " ++ display) ]


experience : Animal -> Html msg
experience dog =
    let
        display =
            (toString dog.experience) ++ " / " ++ ("100")
    in
        div [ class "stats-li stats-experience" ] [ text ("Experience: " ++ display) ]


location : Animal -> Html msg
location dog =
    let
        display =
            toString dog.location
    in
        div [ class "stats-li stats-location" ] [ text ("Location: " ++ display) ]


items : Dict.Dict String Item -> Animal -> Html msg
items items dog =
    let
        display =
            List.map
                (\item ->
                    div [ class "stats-item" ]
                        [ Dict.get item items |> Maybe.map .name |> toString |> text
                        ]
                )
                dog.items
    in
        div [ class "stats-li stats-items" ] ([ text "Items: " ] ++ display)
