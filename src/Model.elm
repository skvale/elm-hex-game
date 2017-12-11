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


type alias Animal =
    { location : Axial
    , imageHref : String
    , key : String
    , health : Int
    , totalHealth : Int
    , experience : Int
    , items : List String
    , moved : Int
    }


type alias Model =
    { board : Grid HexContent
    , size : Float
    , rotateX : Int
    , rotateZ : Int
    , clicked : Maybe Axial
    , scrollX : Int
    , scrollY : Int
    , dogs : Dict.Dict String Animal
    , sheep : Dict.Dict String Animal
    , activeAnimal : String
    , destination : Maybe Axial
    , path : List Axial
    , items : Dict.Dict String Item
    , gate : Axial
    , fence : List Axial
    }


type Msg
    = NoOp
    | Move Keyboard.KeyCode
    | Mover (Axial -> Axial)
    | Click (Tile HexContent)
    | Delete (Tile HexContent)
    | RandomLand Axial Int
    | SetDestination (Tile HexContent)
    | MoveAnimal String Axial Time.Time
    | ColorTile (Tile HexContent)
    | ClickDog Animal
    | ClickSheep Animal


dog3 : Animal
dog3 =
    { baseAnimal
        | location = ( 0, 2 )
        , imageHref = "images/dog3.png"
        , key = "Roy"
        , health = 10
        , totalHealth = 10
    }


dog2 : Animal
dog2 =
    { baseAnimal
        | location = ( 1, 2 )
        , imageHref = "images/dog2.png"
        , key = "Taff"
        , health = 6
        , totalHealth = 6
    }


dog1 : Animal
dog1 =
    { baseAnimal
        | location = ( 1, 1 )
        , imageHref = "images/dog1.png"
        , key = "Wisp"
        , health = 8
        , totalHealth = 8
    }


sheep1 : Animal
sheep1 =
    { baseAnimal
        | location = ( 3, 4 )
        , imageHref = "images/sheep1.png"
        , key = "sheep1"
        , health = 60
        , totalHealth = 60
        , items = []
    }


sheep2 : Animal
sheep2 =
    { baseAnimal
        | location = ( 5, 4 )
        , imageHref = "images/sheep2.png"
        , key = "sheep2"
        , health = 60
        , totalHealth = 60
        , items = []
    }


baseAnimal : Animal
baseAnimal =
    { location = ( 0, 0 )
    , imageHref = ""
    , key = ""
    , health = 0
    , totalHealth = 0
    , experience = 0
    , items = []
    , moved = 0
    }


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
            fillInitialBoardHelper top (i - 1) j (set { dog = Maybe.Nothing, landType = Land } ( i, j ) tmp)
        else
            fillInitialBoardHelper top top (j - 1) (set { dog = Maybe.Nothing, landType = Land } ( i, j ) tmp)
    else
        tmp


fillInitialBoard : Int -> Int -> Grid HexContent
fillInitialBoard i j =
    fillInitialBoardHelper j i j Dict.empty


init : ( Model, Cmd Msg )
init =
    { board =
        fillInitialBoard 20 20
            |> set { dog = Just dog3.key, landType = Land } dog3.location
            |> set { dog = Just dog2.key, landType = Land } dog2.location
            |> set { dog = Just dog1.key, landType = Land } dog1.location
            |> set { dog = Just sheep1.key, landType = Land } sheep1.location
            |> set { dog = Just sheep2.key, landType = Land } sheep2.location
    , size = hexSize
    , rotateX = 0
    , rotateZ = 0
    , clicked = Just dog3.location
    , scrollX = getScrollX dog3.location
    , scrollY = getScrollY dog3.location
    , sheep = Dict.empty |> Dict.insert sheep1.key sheep1 |> Dict.insert sheep2.key sheep2
    , dogs = Dict.empty |> Dict.insert dog2.key dog2 |> Dict.insert dog3.key dog3 |> Dict.insert dog1.key dog1
    , activeAnimal = dog3.key
    , destination = Maybe.Nothing
    , path = []
    , items = Dict.insert "rock" (Item "Rock" []) Dict.empty
    , fence = []
    , gate = ( 6, 6 )
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
                    Dict.get dog3.key model.dogs
                        |> Maybe.withDefault baseAnimal

                attempt =
                    neighborMaker curr.location

                location =
                    if gridContains model.board attempt then
                        attempt
                    else
                        curr.location
            in
                { model
                    | dogs = Dict.insert curr.key { curr | location = location } model.dogs
                    , scrollX = getScrollX location
                    , scrollY = getScrollY location
                }
                    ! []

        Click hex ->
            model
                |> update (SetDestination hex)

        Delete hex ->
            { model | board = model.board } ! []

        -- { model | board = delete hex.coords model.board } ! []
        RandomLand coords i ->
            let
                tile =
                    get model.board coords |> Maybe.withDefault { dog = Maybe.Nothing, landType = Land }

                board =
                    set { tile | landType = getALandType i } coords model.board
            in
                { model | board = board } ! []

        SetDestination hex ->
            { model | destination = Just hex.coords } ! []

        MoveAnimal dog coords time ->
            case model.destination of
                Just somewhere ->
                    let
                        curr =
                            Dict.get dog model.dogs
                                |> Maybe.withDefault baseAnimal

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
                                |> Maybe.withDefault { dog = Maybe.Nothing, landType = Land }

                        prevTile =
                            get model.board curr.location
                                |> Maybe.withDefault { dog = Maybe.Nothing, landType = Land }
                    in
                        case maybeCoord of
                            Just coord ->
                                case tail of
                                    Just rest ->
                                        { model
                                            | dogs = Dict.insert curr.key { curr | location = coord, moved = curr.moved + 1 } model.dogs
                                            , path = rest
                                            , scrollX = getScrollX coord
                                            , scrollY = getScrollY coord
                                            , board =
                                                set { newTile | dog = Just curr.key } coord model.board
                                                    |> set { prevTile | dog = Maybe.Nothing } curr.location
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

        ClickDog dog ->
            case List.length model.path of
                0 ->
                    { model
                        | activeAnimal = dog.key
                        , clicked = Just dog.location
                        , scrollX = getScrollX dog.location
                        , scrollY = getScrollY dog.location
                    }
                        ! []

                _ ->
                    model ! []

        ClickSheep sheep ->
            let
                currAnimal =
                    Dict.get model.activeAnimal model.dogs
                        |> Maybe.withDefault baseAnimal

                newHealth =
                    currAnimal.health - 2
            in
                { model
                    | dogs = Dict.insert currAnimal.key { currAnimal | health = newHealth } model.dogs
                }
                    ! []

        _ ->
            model ! []


getAnimals : Model -> List Animal
getAnimals model =
    let
        dogs =
            Dict.values model.dogs
    in
        dogs
