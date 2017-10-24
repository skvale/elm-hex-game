module Model exposing (..)

import Dict
import Debug
import Hexagons.Main exposing (..)
import Hexagons.Grid exposing (..)
import Hexagons.Path exposing (..)
import Random
import Keyboard
import Time


type LandType
    = Stream
    | Grass
    | Mountain
    | Land


type alias HexContent =
    { players : List Int
    , landType : LandType
    }


type alias Model =
    { board : Grid HexContent
    , size : Float
    , rotateX : Int
    , rotateZ : Int
    , clicked : Maybe Axial
    , scrollX : Int
    , scrollY : Int
    , active : Axial
    , destination : Maybe Axial
    , path : List Axial
    }


type Msg
    = NoOp
    | Move Keyboard.KeyCode
    | Click (Tile HexContent)
    | Delete (Tile HexContent)
    | RandomLand Axial Int
    | SetDestination (Tile HexContent)
    | MoveActive Axial Time.Time
    | ColorTile (Tile HexContent)


startAxial : Axial
startAxial =
    ( 0, 1 )


hexSize : Float
hexSize =
    25


containerWidth : Float
containerWidth =
    550


containerHeight : Float
containerHeight =
    550


getScrollX : Axial -> Int
getScrollX axial =
    axialToPoint hexSize axial |> Tuple.first |> floor |> (*) -1 |> (+) (floor (containerWidth / 2))


getScrollY : Axial -> Int
getScrollY axial =
    axialToPoint hexSize axial |> Tuple.second |> floor |> (*) -1 |> (+) (floor (containerHeight / 2))


getALandType : Int -> LandType
getALandType i =
    List.drop i [ Stream, Grass, Mountain ] |> List.head |> Maybe.withDefault Land


fillInitialBoardHelper : Int -> Int -> Int -> Grid HexContent -> Grid HexContent
fillInitialBoardHelper top i j tmp =
    if j > 0 then
        if i > 0 then
            fillInitialBoardHelper top (i - 1) j (set { players = [], landType = Land } ( i, j ) tmp)
        else
            fillInitialBoardHelper top top (j - 1) (set { players = [], landType = Land } ( i, j ) tmp)
    else
        tmp


fillInitialBoard : Int -> Int -> Grid HexContent
fillInitialBoard i j =
    fillInitialBoardHelper j i j Dict.empty


init : ( Model, Cmd Msg )
init =
    { board = fillInitialBoard 20 20
    , size = hexSize
    , rotateX = 0
    , rotateZ = 0
    , clicked = Maybe.Nothing
    , scrollX = getScrollX startAxial
    , scrollY = getScrollY startAxial
    , active = startAxial
    , destination = Maybe.Nothing
    , path = []
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "" msg) of
        Move direction ->
            case direction of
                68 ->
                    let
                        attempt =
                            ( (Tuple.first model.active) + 1, Tuple.second model.active )

                        active =
                            if gridContains model.board attempt then
                                attempt
                            else
                                model.active
                    in
                        { model
                            | active = active
                            , scrollX = getScrollX active
                            , scrollY = getScrollY active
                        }
                            ! []

                65 ->
                    let
                        attempt =
                            ( (Tuple.first model.active) - 1, Tuple.second model.active )

                        active =
                            if gridContains model.board attempt then
                                attempt
                            else
                                model.active
                    in
                        { model
                            | active = active
                            , scrollX = getScrollX active
                            , scrollY = getScrollY active
                        }
                            ! []

                83 ->
                    let
                        attempt =
                            ( (Tuple.first model.active), Tuple.second model.active + 1 )

                        active =
                            if gridContains model.board attempt then
                                attempt
                            else
                                model.active
                    in
                        { model
                            | active = active
                            , scrollX = getScrollX active
                            , scrollY = getScrollY active
                        }
                            ! []

                87 ->
                    let
                        attempt =
                            ( (Tuple.first model.active), Tuple.second model.active - 1 )

                        active =
                            if gridContains model.board attempt then
                                attempt
                            else
                                model.active
                    in
                        { model
                            | active = active
                            , scrollX = getScrollX active
                            , scrollY = getScrollY active
                        }
                            ! []

                _ ->
                    model ! []

        Click hex ->
            model
                |> update (SetDestination hex)
                |> Tuple.first
                |> update (ColorTile hex)

        Delete hex ->
            { model | board = model.board } ! []

        -- { model | board = delete hex.coords model.board } ! []
        RandomLand coords i ->
            let
                tile =
                    get model.board coords |> Maybe.withDefault { players = [], landType = Land }

                board =
                    set { tile | landType = getALandType i } coords model.board
            in
                { model | board = board } ! []

        SetDestination hex ->
            { model | destination = Just hex.coords } ! []

        MoveActive coords time ->
            case model.destination of
                Just somewhere ->
                    let
                        path =
                            if List.isEmpty model.path then
                                case getPath model.board model.active coords of
                                    Just aPath ->
                                        aPath

                                    _ ->
                                        model.path
                            else
                                model.path

                        maybeCoord =
                            List.head path

                        tail =
                            List.tail path
                    in
                        case maybeCoord of
                            Just coord ->
                                case tail of
                                    Just rest ->
                                        { model
                                            | active = coord
                                            , path = rest
                                            , scrollX = getScrollX coord
                                            , scrollY = getScrollY coord
                                        }
                                            ! []

                                    _ ->
                                        model ! []

                            _ ->
                                { model
                                    | destination = Maybe.Nothing
                                    , path = []
                                }
                                    ! []

                _ ->
                    model ! []

        ColorTile hex ->
            { model | clicked = Just hex.coords }
                ! (if hex.content.landType == Land then
                    [ Random.generate (RandomLand hex.coords) (Random.int 0 2) ]
                   else
                    []
                  )

        _ ->
            model ! []
