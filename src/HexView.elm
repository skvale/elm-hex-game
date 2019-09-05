module HexView exposing (viewHexagons)

import Dict
import Hexagons.Grid exposing (..)
import Hexagons.HexContent exposing (..)
import Hexagons.Main exposing (..)
import Html exposing (div)
import Model exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)


matrixTransform : Model -> String
matrixTransform model =
    "matrix(1, 0, 0, 1," ++ String.fromInt model.scrollX ++ "," ++ String.fromInt model.scrollY ++ ")"


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
            List.map view hexagons ++ List.map (viewDog model) dogs ++ List.map (viewSheep model) sheep ++ renderPaths model
    in
    div [ class "hexagons" ]
        [ svg
            [ width <| (floor containerWidth |> String.fromInt) ++ "px"
            , height <| (floor containerHeight |> String.fromInt) ++ "px"
            ]
            [ defs [] (List.map dog dogs ++ List.map renderSheep sheep)
            , g
                [ transform (matrixTransform model), class "svg-polygons" ]
                allHexes
            ]
        ]


renderPaths : Model -> List (Svg msg)
renderPaths model =
    List.concat (List.map (renderPath model) (Dict.values model.dogs))


renderPath : Model -> Animal -> List (Svg msg)
renderPath model d =
    List.map (renderPathPoint model.size) d.path


renderPathPoint : Float -> Axial -> Svg msg
renderPathPoint size axial =
    let
        center =
            axialToPoint size axial
    in
    circle [ r "2", cx <| String.fromFloat <| Tuple.first center + size, cy <| String.fromFloat <| Tuple.second center + size ] []


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
            [ Tuple.first center + size + size * cos radians
            , Tuple.second center + size + size * sin radians
            ]
    in
    String.join "," (List.map String.fromFloat coords)


viewHex : Float -> Maybe Axial -> Hexagons.Grid.Tile HexContent -> Svg Msg
viewHex size clicked tile =
    polygon
        [ points (hexPoints tile.coords size)
        , class
            (landTypeToClass tile.content.landType
                ++ " polygon-land "
                ++ (if axialIsEqual clicked (Just tile.coords) then
                        "selected-tile "

                    else
                        ""
                   )
                ++ "height-" ++ (String.fromInt tile.height)
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
dog d =
    pattern [ id d.key, width "100%", height "100%" ]
        [ image [ xlinkHref d.imageHref, class "dog", width "50", height "50", preserveAspectRatio "none" ] []
        ]


renderSheep : Animal -> Svg msg
renderSheep sheep =
    pattern [ id sheep.key, width "100%", height "100%" ]
        [ image [ xlinkHref sheep.imageHref, class "sheep", width "50", height "50", preserveAspectRatio "none" ] []
        ]


viewDog : Model -> Animal -> Svg Msg
viewDog model d =
    polygon
        [ points (hexPoints d.location model.size)
        , onClick <| ClickDog d
        , class "polygon-dog"
        , fill ("url(#" ++ d.key ++ ")")
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
