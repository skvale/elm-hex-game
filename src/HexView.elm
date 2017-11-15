module HexView exposing (viewHexagons)

import Dict
import Html exposing (div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Hexagons.Main exposing (..)
import Hexagons.Grid exposing (..)
import Model exposing (..)


matrixTransform : Model -> String
matrixTransform model =
    "matrix(1, 0, 0, 1," ++ (toString model.scrollX) ++ "," ++ (toString model.scrollY) ++ ")"


viewHexagons : Model -> Svg Msg
viewHexagons model =
    let
        view =
            viewHex model.size model.clicked

        hexagons =
            Hexagons.Grid.list model.board

        characters =
            Dict.values model.characters
    in
        div [ class "hexagons" ]
            [ svg
                [ width <| (floor containerWidth |> toString) ++ "px"
                , height <| (floor containerHeight |> toString) ++ "px"
                ]
                [ g
                    [ transform (matrixTransform model), class "svg-polygons" ]
                    ((List.map (viewCharacter model) characters) ++ (List.map view hexagons))
                ]
            ]


hexPoints : Hexagons.Grid.Tile HexContent -> Float -> String
hexPoints tile size =
    let
        getCornerPoint =
            hexCorner tile size
    in
        String.join " " (List.map getCornerPoint (List.range 1 6))


hexCorner : Hexagons.Grid.Tile HexContent -> Float -> Int -> String
hexCorner tile size idx =
    let
        degree =
            60.0 * toFloat idx + 30.0

        radians =
            pi / 180 * degree

        center =
            axialToPoint size tile.coords

        coords =
            [ Tuple.first center + size + size * cos (radians)
            , Tuple.second center + size + size * sin (radians)
            ]
    in
        String.join "," (List.map toString coords)


viewHex : Float -> Maybe Axial -> Hexagons.Grid.Tile HexContent -> Svg Msg
viewHex size clicked tile =
    polygon
        [ points (hexPoints tile size)
        , class
            (landTypeToClass tile.content.landType
                ++ " "
                ++ if axialIsEqual clicked (Just tile.coords) then
                    "selected-tilef "
                   else
                    ""
            )
        , onClick <| Click tile
        , Svg.Events.onMouseOver <| Delete tile
        ]
        []


viewCharacter : Model -> Character -> Svg Msg
viewCharacter model character =
    g [ transform ("translate" ++ toString (axialToPoint model.size character.location)) ]
        [ image
            [ xlinkHref character.imageHref
            , width "40px"
            , height "40px"
            , class "character"
            ]
            []
        ]


landTypeToClass : LandType -> String
landTypeToClass landType =
    case landType of
        Stream ->
            "stream"

        Grass ->
            "grass"

        Mountain ->
            "mountain"

        _ ->
            "land"
