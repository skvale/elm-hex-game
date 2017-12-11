module Hexagons.HexContent exposing (..)

import Hexagons.Grid exposing (..)
import Hexagons.Main exposing (..)


type LandType
    = Stream
    | Grass
    | Mountain
    | Land


type alias HexContent =
    { dog : Maybe String
    , landType : LandType
    }
