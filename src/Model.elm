module Model exposing (..)

import Dict
import Debug
import Hexagons.Main exposing (..)
import Hexagons.Grid exposing (..)
import Hexagons.Path exposing (..)
import Random
import Keyboard
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
    , path : List Axial
    , destination : Maybe Axial
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
    , activeDog : String
    , items : Dict.Dict String Item
    , gate : Axial
    , fence : List Axial
    , turnEnd : Bool
    }


type Msg
    = Click (Tile HexContent)
    | Delete (Tile HexContent)
    | RandomLand Axial Int
    | SetDestination (Tile HexContent)
    | ColorTile (Tile HexContent)
    | ClickDog Animal
    | ClickSheep Animal
    | TurnEnd


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
    , path = []
    , destination = Maybe.Nothing
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
    , activeDog = dog3.key
    , items = Dict.insert "rock" (Item "Rock" []) Dict.empty
    , fence = []
    , gate = ( 6, 6 )
    , turnEnd = False
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "" msg) of
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
            let
                curr =
                    Dict.get model.activeDog model.dogs
                        |> Maybe.withDefault baseAnimal

                path =
                    case getPath model.board curr.location hex.coords of
                        Just aPath ->
                            aPath

                        _ ->
                            []
            in
                { model
                    | dogs = Dict.insert curr.key { curr | path = path, destination = Just hex.coords } model.dogs
                }
                    ! []

        ColorTile hex ->
            { model | clicked = Just hex.coords }
                ! (if hex.content.landType == Land then
                    [ Random.generate (RandomLand hex.coords) (Random.int 0 2) ]
                   else
                    []
                  )

        ClickDog dog ->
            { model
                | activeDog = dog.key
                , clicked = Just dog.location
                , scrollX = getScrollX dog.location
                , scrollY = getScrollY dog.location
            }
                ! []

        ClickSheep sheep ->
            let
                currAnimal =
                    Dict.get model.activeDog model.dogs
                        |> Maybe.withDefault baseAnimal

                newHealth =
                    currAnimal.health - 2
            in
                { model
                    | dogs = Dict.insert currAnimal.key { currAnimal | health = newHealth } model.dogs
                }
                    ! []

        TurnEnd ->
            let
                dogIdList =
                    Dict.toList model.dogs

                dogList =
                    List.map (\( id, dog ) -> ( id, moveDog model dog )) dogIdList

                dogMap =
                    Dict.fromList dogList
            in
                { model | dogs = dogMap } ! []


moveDog : Model -> Animal -> Animal
moveDog model dog =
    case dog.destination of
        Just coords ->
            case List.head dog.path of
                Just nextCoord ->
                    case List.tail dog.path of
                        Just rest ->
                            { dog
                                | location = nextCoord
                                , moved = dog.moved + 1
                                , path = rest
                            }

                        _ ->
                            dog

                _ ->
                    dog

        _ ->
            dog
