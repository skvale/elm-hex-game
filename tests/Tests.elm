module Tests exposing (..)

import Test exposing (..)
import Expect

all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        ]
