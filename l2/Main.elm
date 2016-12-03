module Main exposing (..)

import Keypad exposing (Model, Msg, init, update, subscriptions, view)
import Html exposing (program)


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
