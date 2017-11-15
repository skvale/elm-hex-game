module Stats exposing (viewStats)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)


viewStats : Model -> Html msg
viewStats model =
  let
      character =
        Dict.get model.activeCharacter model.characters
  in

    div [ class "stats" ]
        [ header
        , activePlayer character
        , health character
        , magic character
        , experience character
        ]


header : Html msg
header =
    div [ class "stats-header" ]
        [ text "Game stats" ]


activePlayer : Maybe Character -> Html msg
activePlayer maybeCharacter =
  let
        display =
          case maybeCharacter of
            Just character ->
                character.key
            _ ->
              "None"
  in
      div [ class "stats-active-player" ] [ text ("Active: " ++ display) ]

health : Maybe Character -> Html msg
health maybeCharacter =
  let
        display =
          case maybeCharacter of
            Just character ->
                (toString character.health) ++ " / " ++ (toString character.totalHealth)
            _ ->
              "-"
  in
      div [ class "stats-health" ] [ text ("Health: " ++ display) ]

magic : Maybe Character -> Html msg
magic maybeCharacter =
  let
        display =
          case maybeCharacter of
            Just character ->
                (toString character.magic) ++ " / " ++ (toString character.totalMagic)
            _ ->
              "-"
  in
      div [ class "stats-magic" ] [ text ("Magic: " ++ display) ]


experience : Maybe Character -> Html msg
experience maybeCharacter =
  let
        display =
          case maybeCharacter of
            Just character ->
                (toString character.experience) ++ " / " ++ ("100")
            _ ->
              "-"
  in
      div [ class "stats-magic" ] [ text ("Experience: " ++ display) ]

