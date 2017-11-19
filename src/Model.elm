module Model exposing (..)

import Dict
import Debug
import Hexagons.Main exposing (..)
import Hexagons.Grid exposing (..)
import Hexagons.Path exposing (..)
import Random
import Keyboard
import Time
import Hexagons.HexContent exposing (..)


type alias Item =
    { name : String
    , attributes : List String
    }

type alias Character =
    { location : Axial
    , imageHref : String
    , key : String
    , health : Int
    , totalHealth : Int
    , magic : Int
    , totalMagic : Int
    , experience : Int
    , items : List String
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
    , items : Dict.Dict String Item
    , baddies : Dict.Dict String Character
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
    | ClickCharacter Character
    | ClickBaddie Character


human : Character
human =
    { stubCharacter
        | location = ( 0, 2 )
        , imageHref = "images/human.png"
        , key = "Brutus"
        , health = 10
        , totalHealth = 10
        , magic = 4
        , totalMagic = 4
        , items = [ "rock" ]
    }


wizard : Character
wizard =
    { stubCharacter
        | location = startAxial
        , imageHref = "images/wizard.png"
        , key = "Marius"
        , health = 6
        , totalHealth = 6
        , magic = 12
        , totalMagic = 12
    }


thief : Character
thief =
    { stubCharacter
        | location = ( 1, 1 )
        , imageHref = "images/thief.png"
        , key = "Farda"
        , health = 8
        , totalHealth = 8
        , magic = 3
        , totalMagic = 3
    }


dragon : Character
dragon =
    { location = ( 3, 4 )
    , imageHref = "images/dragon.png"
    , key = "Dragon"
    , health = 60
    , totalHealth = 60
    , magic = 120
    , totalMagic = 120
    , experience = 0
    , items = [ "gold" ]
    }


stubCharacter : Character
stubCharacter =
    { location = startAxial
    , imageHref = ""
    , key = ""
    , health = 0
    , totalHealth = 0
    , magic = 0
    , totalMagic = 0
    , experience = 0
    , items = []
    }


startAxial : Axial
startAxial =
    ( 0, 1 )


hexSize : Float
hexSize =
    25


containerWidth : Float
containerWidth =
    650


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
            fillInitialBoardHelper top (i - 1) j (set { character = Maybe.Nothing, landType = Land } ( i, j ) tmp)
        else
            fillInitialBoardHelper top top (j - 1) (set { character = Maybe.Nothing, landType = Land } ( i, j ) tmp)
    else
        tmp


fillInitialBoard : Int -> Int -> Grid HexContent
fillInitialBoard i j =
    fillInitialBoardHelper j i j Dict.empty


init : ( Model, Cmd Msg )
init =
    { board =
        fillInitialBoard 20 20
            |> set { character = Just human.key, landType = Land } human.location
            |> set { character = Just wizard.key, landType = Land } wizard.location
            |> set { character = Just thief.key, landType = Land } thief.location
            |> set { character = Just dragon.key, landType = Land } dragon.location
    , size = hexSize
    , rotateX = 0
    , rotateZ = 0
    , clicked = Just human.location
    , scrollX = getScrollX human.location
    , scrollY = getScrollY human.location
    , baddies = Dict.insert dragon.key dragon Dict.empty
    , characters = Dict.empty |> Dict.insert wizard.key wizard |> Dict.insert human.key human |> Dict.insert thief.key thief
    , activeCharacter = human.key
    , destination = Maybe.Nothing
    , path = []
    , items = Dict.insert "rock" (Item "Rock" []) Dict.empty
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
                    get model.board coords |> Maybe.withDefault { character = Maybe.Nothing, landType = Land }

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

                        newTile =
                            get model.board coords
                                |> Maybe.withDefault { character = Maybe.Nothing, landType = Land }

                        prevTile =
                            get model.board curr.location
                                |> Maybe.withDefault { character = Maybe.Nothing, landType = Land }
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
                                            , board =
                                                set { newTile | character = Just curr.key } coord model.board
                                                    |> set { prevTile | character = Maybe.Nothing } curr.location
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

        ClickCharacter character ->
            case List.length model.path of
                0 ->
                    { model
                        | activeCharacter = character.key
                        , clicked = Just character.location
                        , scrollX = getScrollX character.location
                        , scrollY = getScrollY character.location
                    }
                        ! []
                _ ->
                    model ! []

        ClickBaddie baddie ->
            let
                currCharacter =
                    Dict.get model.activeCharacter model.characters
                        |> Maybe.withDefault stubCharacter

                newHealth =
                    currCharacter.health - 2
            in
                { model
                    | characters = Dict.insert currCharacter.key { currCharacter | health = newHealth } model.characters
                }
                    ! []

        _ ->
            model ! []
