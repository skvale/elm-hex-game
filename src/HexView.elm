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

        characters =
            Dict.values model.characters

        baddies =
            Dict.values model.baddies
    in
        div [ class "hexagons" ]
            [ svg
                [ width <| (floor containerWidth |> toString) ++ "px"
                , height <| (floor containerHeight |> toString) ++ "px"
                ]
                [ defs [] ((List.map character characters) ++ (List.map baddie baddies))
                , g
                    [ transform (matrixTransform model), class "svg-polygons" ]
                    ((List.map view hexagons) ++ (List.map (viewCharacter model) characters) ++ (List.map (viewBaddie model) baddies))
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


character : Character -> Svg msg
character character =
    pattern [ id character.key, width "100%", height "100%" ]
        [ image [ xlinkHref character.imageHref, class "character", width "50", height "50", preserveAspectRatio "none" ] []
        ]

baddie : Character -> Svg msg
baddie baddie =
    pattern [ id baddie.key, width "100%", height "100%" ]
        [ image [ xlinkHref baddie.imageHref, class "baddie", width "50", height "50", preserveAspectRatio "none" ] []
        ]

viewCharacter : Model -> Character -> Svg Msg
viewCharacter model character =
    polygon
        [ points (hexPoints character.location model.size)
        , onClick <| ClickCharacter character
        , class "polygon-character"
        , fill ("url(#" ++ character.key ++ ")")
        ]
        []

viewBaddie : Model -> Character -> Svg Msg
viewBaddie model baddie =
    polygon
        [ points (hexPoints baddie.location model.size)
        , class "polygon-baddie"
        , ClickBaddie baddie |> onClick
        , fill ("url(#" ++ baddie.key ++ ")")
        ]
        []