module Hexagons.HexContent exposing (..)

import Hexagons.Grid exposing (..)
import Hexagons.Main exposing (..)


type LandType
    = Stream
    | Grass
    | Mountain
    | Land


type alias HexContent =
    { character : Maybe String
    , landType : LandType
    }
