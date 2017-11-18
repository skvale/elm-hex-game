module Hexagons.Path exposing (getPath)

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Tuple exposing (first, second)
import Hexagons.Main exposing (..)
import Hexagons.Grid exposing (..)
import Hexagons.HexContent exposing (..)


{--Taken from https://github.com/krisajenkins/elm-astar
The A* pathfinding algorithm for Elm. by Kris Jenkins

--}


type alias Path =
    List Axial


getPath :
    Grid HexContent
    -> Axial
    -> Axial
    -> Maybe Path
getPath grid start end =
    initialModel start
        |> astar pythagoreanCost (axialNeighbors grid) end
        |> Maybe.map Array.toList


hasCharacter : Grid HexContent -> Axial -> Bool
hasCharacter grid axial =
    let
        maybeHexContent =
            Hexagons.Grid.get grid axial
    in
        case maybeHexContent of
            Just hexContent ->
                case hexContent.character of
                    Just c ->
                        False

                    _ ->
                        True

            _ ->
                True


axialNeighbors : Grid HexContent -> Axial -> Set Axial
axialNeighbors grid a =
    [ ( Tuple.first a, Tuple.second a - 1 )
    , ( Tuple.first a + 1, Tuple.second a - 1 )
    , ( Tuple.first a + 1, Tuple.second a )
    , ( Tuple.first a, Tuple.second a + 1 )
    , ( Tuple.first a - 1, Tuple.second a + 1 )
    , ( Tuple.first a - 1, Tuple.second a )
    ]
        |> List.filter (gridContains grid)
        |> List.filter (hasCharacter grid)
        |> Set.fromList


pythagoreanCost : Axial -> Axial -> Float
pythagoreanCost ( x1, y1 ) ( x2, y2 ) =
    let
        dx =
            toFloat <| abs (x1 - x2)

        dy =
            toFloat <| abs (y1 - y2)
    in
        (sqrt 2 * min dx dy) + abs (dy - dx) |> abs


type alias Model comparable =
    { evaluated : Set comparable
    , openSet : Set comparable
    , costs : Dict comparable Float
    , cameFrom : Dict comparable comparable
    }


initialModel : comparable -> Model comparable
initialModel start =
    { evaluated = Set.empty
    , openSet = Set.singleton start
    , costs = Dict.singleton start 0
    , cameFrom = Dict.empty
    }


cheapestOpen : (comparable -> Float) -> Model comparable -> Maybe comparable
cheapestOpen costFn model =
    model.openSet
        |> Set.toList
        |> List.filterMap
            (\position ->
                case Dict.get position model.costs of
                    Nothing ->
                        Nothing

                    Just cost ->
                        Just ( position, cost + costFn position )
            )
        |> List.sortBy second
        |> List.head
        |> Maybe.map first


reconstructPath : Dict comparable comparable -> comparable -> Array comparable
reconstructPath cameFrom goal =
    case Dict.get goal cameFrom of
        Nothing ->
            Array.empty

        Just next ->
            Array.push goal
                (reconstructPath cameFrom next)


updateCost : comparable -> comparable -> Model comparable -> Model comparable
updateCost current neighbour model =
    let
        newCameFrom =
            Dict.insert neighbour current model.cameFrom

        newCosts =
            Dict.insert neighbour distanceTo model.costs

        distanceTo =
            reconstructPath newCameFrom neighbour
                |> Array.length
                |> toFloat

        newModel =
            { model
                | costs = newCosts
                , cameFrom = newCameFrom
            }
    in
        case Dict.get neighbour model.costs of
            Nothing ->
                newModel

            Just previousDistance ->
                if distanceTo < previousDistance then
                    newModel
                else
                    model


astar :
    (comparable -> comparable -> Float)
    -> (comparable -> Set comparable)
    -> comparable
    -> Model comparable
    -> Maybe (Array comparable)
astar costFn moveFn goal model =
    case cheapestOpen (costFn goal) model of
        Nothing ->
            Nothing

        Just current ->
            if current == goal then
                Just (reconstructPath model.cameFrom goal)
            else
                let
                    modelPopped =
                        { model
                            | openSet = Set.remove current model.openSet
                            , evaluated = Set.insert current model.evaluated
                        }

                    neighbours =
                        moveFn current

                    newNeighbours =
                        Set.diff neighbours modelPopped.evaluated

                    modelWithNeighbours =
                        { modelPopped
                            | openSet =
                                Set.union modelPopped.openSet
                                    newNeighbours
                        }

                    modelWithCosts =
                        Set.foldl (updateCost current) modelWithNeighbours newNeighbours
                in
                    astar costFn moveFn goal modelWithCosts
