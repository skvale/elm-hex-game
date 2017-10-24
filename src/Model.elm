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
    , characters : Dict.Dict String Character
    , activeCharacter : String
    , destination : Maybe Axial
    , path : List Axial
    }


type alias Character =
    { location : Axial
    , imageHref : String
    , key : String
    }


type Msg
    = NoOp
    | Move Keyboard.KeyCode
    | Mover (Axial -> Axial)
    | Click (Tile HexContent)
    | Delete (Tile HexContent)
    | RandomLand Axial Int
    | SetDestination (Tile HexContent)
    | MoveCharacter String Axial Time.Time
    | ColorTile (Tile HexContent)


human : Character
human =
    { location = startAxial, imageHref = "/images/human.png", key = "human" }


wizard : Character
wizard =
    { location = startAxial, imageHref = "/images/wizard.png", key = "wizard" }


stubCharacter : Character
stubCharacter =
    { location = startAxial, imageHref = "", key = "" }


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
    , characters = Dict.insert wizard.key wizard (Dict.insert human.key human Dict.empty)
    , activeCharacter = "human"
    , destination = Maybe.Nothing
    , path = []
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "" msg) of
        Move key ->
            case key of
                65 ->
                    -- a
                    update (Mover (\axial -> ( (Tuple.first axial) - 1, Tuple.second axial ))) model

                67 ->
                    -- c
                    update (Mover (\axial -> ( (Tuple.first axial), Tuple.second axial + 1 ))) model

                68 ->
                    -- d
                    update (Mover (\axial -> ( (Tuple.first axial) + 1, Tuple.second axial ))) model

                69 ->
                    -- e
                    update (Mover (\axial -> ( (Tuple.first axial) + 1, Tuple.second axial - 1 ))) model

                81 ->
                    -- q
                    update (Mover (\axial -> ( (Tuple.first axial), Tuple.second axial - 1 ))) model

                90 ->
                    -- z
                    update (Mover (\axial -> ( (Tuple.first axial) - 1, Tuple.second axial + 1 ))) model

                _ ->
                    model ! []

        Mover neighborMaker ->
            let
                curr =
                    Dict.get human.key model.characters
                        |> Maybe.withDefault stubCharacter

                attempt =
                    neighborMaker curr.location

                location =
                    if gridContains model.board attempt then
                        attempt
                    else
                        curr.location
            in
                { model
                    | characters = Dict.insert curr.key { curr | location = location } model.characters
                    , scrollX = getScrollX location
                    , scrollY = getScrollY location
                }
                    ! []

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

        MoveCharacter character coords time ->
            case model.destination of
                Just somewhere ->
                    let
                        curr =
                            Dict.get character model.characters
                                |> Maybe.withDefault stubCharacter

                        path =
                            if List.isEmpty model.path then
                                case getPath model.board curr.location coords of
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
                                            | characters = Dict.insert curr.key { curr | location = coord } model.characters
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
