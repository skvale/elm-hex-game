module HexView exposing (viewHexagons)

import Dict
import Html exposing (div)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Hexagons.Main exposing (..)
import Hexagons.Grid exposing (..)
import Hexagons.HexContent exposing (..)
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

        dogs =
            Dict.values model.dogs

        sheep =
            Dict.values model.sheep

        allHexes =
            (List.map view hexagons) ++ (List.map (viewAnimal model) dogs) ++ (List.map (viewSheep model) sheep)

    in
        div [ class "hexagons" ]
            [ svg
                [ width <| (floor containerWidth |> toString) ++ "px"
                , height <| (floor containerHeight |> toString) ++ "px"
                ]
                [ defs [] ((List.map dog dogs) ++ (List.map renderSheep sheep))
                , g
                    [ transform (matrixTransform model), class "svg-polygons" ]
                    allHexes
                ]
            ]


hexPoints : Axial -> Float -> String
hexPoints axial size =
    let
        getCornerPoint =
            hexCorner axial size
    in
        String.join " " (List.map getCornerPoint (List.range 1 6))


hexCorner : Axial -> Float -> Int -> String
hexCorner axial size idx =
    let
        degree =
            60.0 * toFloat idx + 30.0

        radians =
            pi / 180 * degree

        center =
            axialToPoint size axial

        coords =
            [ Tuple.first center + size + size * cos (radians)
            , Tuple.second center + size + size * sin (radians)
            ]
    in
        String.join "," (List.map toString coords)


viewHex : Float -> Maybe Axial -> Hexagons.Grid.Tile HexContent -> Svg Msg
viewHex size clicked tile =
    polygon
        [ points (hexPoints tile.coords size)
        , class
            (landTypeToClass tile.content.landType
                ++ " polygon-land "
                ++ if axialIsEqual clicked (Just tile.coords) then
                    "selected-tile "
                   else
                    ""
            )
        , onClick <| Click tile
        , Svg.Events.onMouseOver <| Delete tile
        ]
        []


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


dog : Animal -> Svg msg
dog dog =
    pattern [ id dog.key, width "100%", height "100%" ]
        [ image [ xlinkHref dog.imageHref, class "dog", width "50", height "50", preserveAspectRatio "none" ] []
        ]


renderSheep : Animal -> Svg msg
renderSheep sheep =
    pattern [ id sheep.key, width "100%", height "100%" ]
        [ image [ xlinkHref sheep.imageHref, class "sheep", width "50", height "50", preserveAspectRatio "none" ] []
        ]


viewAnimal : Model -> Animal -> Svg Msg
viewAnimal model dog =
    polygon
        [ points (hexPoints dog.location model.size)
        , onClick <| ClickDog dog
        , class "polygon-dog"
        , fill ("url(#" ++ dog.key ++ ")")
        ]
        []


viewSheep : Model -> Animal -> Svg Msg
viewSheep model sheep =
    polygon
        [ points (hexPoints sheep.location model.size)
        , class "polygon-sheep"
        , ClickSheep sheep |> onClick
        , fill ("url(#" ++ sheep.key ++ ")")
        ]
        []
