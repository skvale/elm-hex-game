module Main exposing (..)

import Html exposing (program, Html)
import Html.Attributes exposing (..)
import Model exposing (..)
import HexView exposing (..)
import Keyboard
import Time exposing (every, millisecond)


view : Model -> Html Msg
view model =
    Html.div [ class "app" ]
        [ viewHexagons model ]


main : Program Never Model Msg
main =
    program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    (case model.destination of
        Just coords ->
            [ every (200 * millisecond) (MoveActive coords) ]

        _ ->
            []
    )
        |> List.append [ Keyboard.ups Move ]
        |> Sub.batch
