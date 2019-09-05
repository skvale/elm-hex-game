module Stats exposing (viewStats)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)


viewStats : Model -> Html msg
viewStats model =
    let
        maybeAnimal =
            Dict.get model.activeDog model.dogs
    in
    case maybeAnimal of
        Just dog ->
            div [ class "stats" ]
                [ header
                , activeDog dog
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


activeDog : Animal -> Html msg
activeDog dog =
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
            String.fromInt dog.health ++ " / " ++ String.fromInt dog.totalHealth
    in
    div [ class "stats-li stats-health" ] [ text ("Health: " ++ display) ]


experience : Animal -> Html msg
experience dog =
    let
        display =
            String.fromInt dog.experience ++ " / " ++ "100"
    in
    div [ class "stats-li stats-experience" ] [ text ("Experience: " ++ display) ]


location : Animal -> Html msg
location dog =
    let
        display =
            Debug.toString dog.location
    in
    div [ class "stats-li stats-location" ] [ text ("Location: " ++ display) ]


items : Dict.Dict String Item -> Animal -> Html msg
items theItems dog =
    let
        display =
            List.map
                (\item ->
                    div [ class "stats-item" ]
                        [ Dict.get item theItems |> Maybe.map .name |> Debug.toString |> text
                        ]
                )
                dog.items
    in
    div [ class "stats-li stats-items" ] ([ text "Items: " ] ++ display)
