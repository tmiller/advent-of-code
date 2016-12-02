module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)


type Direction
    = North
    | South
    | East
    | West


type Instruction
    = Right Int
    | Left Int


type alias Location =
    { x : Int, y : Int }


type alias Model =
    { actor : Actor
    , instructions : List Instruction
    }


type alias Actor =
    { direction : Direction
    , location : Location
    }


type Msg
    = NoOp
    | Travel
    | Step


initModel : Model
initModel =
    Model (Actor North (Location 0 0)) instSet4


main =
    beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }


turn : Instruction -> Actor -> ( Actor, Int )
turn inst actor =
    let
        leftMapping =
            case actor.direction of
                North ->
                    West

                South ->
                    East

                East ->
                    North

                West ->
                    South

        rightMapping =
            case actor.direction of
                North ->
                    East

                South ->
                    West

                East ->
                    South

                West ->
                    North

        ( newDirection, blocks ) =
            case inst of
                Left blocks ->
                    ( leftMapping, blocks )

                Right blocks ->
                    ( rightMapping, blocks )

        newActor =
            { actor | direction = newDirection }
    in
        ( newActor, blocks )


move : Actor -> Int -> Actor
move actor blocks =
    let
        location =
            actor.location

        newLocation =
            case actor.direction of
                North ->
                    { location | y = location.y + blocks }

                South ->
                    { location | y = location.y - blocks }

                East ->
                    { location | x = location.x + blocks }

                West ->
                    { location | x = location.x - blocks }
    in
        { actor | location = newLocation }


step : Instruction -> Actor -> Actor
step inst =
    turn inst >> uncurry move


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Travel ->
            { model
                | actor = List.foldl step model.actor model.instructions
                , instructions = []
            }

        Step ->
            case model.instructions of
                x :: xs ->
                    { model
                        | actor = step x model.actor
                        , instructions = xs
                    }

                [] ->
                    model


view : Model -> Html Msg
view model =
    let
        actor =
            model.actor
    in
        div []
            [ button [ onClick Step ] [ text "Step" ]
            , button [ onClick Travel ] [ text "Travel" ]
            , viewActor "Current" actor
            , viewActor "Original" initModel.actor
            , div [] [ text ("Distance: " ++ (showDistance actor.location)) ]
            ]


viewActor : String -> Actor -> Html Msg
viewActor name actor =
    let
        x =
            "Y: " ++ (toString actor.location.x)

        y =
            "X: " ++ (toString actor.location.y)

        direction =
            "Dir: " ++ (toString actor.direction)

        spanStyle =
            style [ ( "margin-right", "10px" ) ]

        containerStyle =
            style [ ( "width", "200px" ), ( "margin", "10px" ) ]
    in
        div
            [ containerStyle ]
            [ div [] [ text name ]
            , span [ spanStyle ] [ text y ]
            , span [ spanStyle ] [ text x ]
            , span [ spanStyle ] [ text direction ]
            ]


showDistance : Location -> String
showDistance loc =
    let
        x =
            abs loc.x

        y =
            abs loc.y
    in
        toString (x + y)


instSet1 : List Instruction
instSet1 =
    [ Right 2
    , Left 3
    ]


instSet2 : List Instruction
instSet2 =
    [ Right 2
    , Right 2
    , Right 2
    ]


instSet3 : List Instruction
instSet3 =
    [ Right 5
    , Left 5
    , Right 5
    , Right 3
    ]


instSet4 : List Instruction
instSet4 =
    [ Left 4
    , Left 3
    , Right 1
    , Left 4
    , Right 2
    , Right 2
    , Left 1
    , Left 2
    , Right 1
    , Right 1
    , Left 3
    , Right 5
    , Left 2
    , Right 5
    , Left 4
    , Left 3
    , Right 2
    , Right 2
    , Left 5
    , Left 1
    , Right 4
    , Left 1
    , Right 3
    , Left 3
    , Right 5
    , Right 2
    , Left 5
    , Right 2
    , Right 1
    , Right 1
    , Left 5
    , Right 1
    , Left 3
    , Left 2
    , Left 5
    , Right 4
    , Right 4
    , Left 2
    , Left 1
    , Left 1
    , Right 1
    , Right 1
    , Left 185
    , Right 4
    , Left 1
    , Left 1
    , Right 5
    , Right 1
    , Left 1
    , Left 3
    , Left 2
    , Left 1
    , Right 2
    , Right 2
    , Right 2
    , Left 1
    , Left 1
    , Right 4
    , Right 5
    , Right 53
    , Left 1
    , Right 1
    , Right 78
    , Right 3
    , Right 4
    , Left 1
    , Right 5
    , Left 1
    , Left 4
    , Right 3
    , Right 3
    , Left 3
    , Left 3
    , Right 191
    , Right 4
    , Right 1
    , Left 4
    , Left 1
    , Right 3
    , Left 1
    , Left 2
    , Right 3
    , Right 2
    , Right 4
    , Right 5
    , Right 5
    , Left 3
    , Left 5
    , Right 2
    , Right 3
    , Left 1
    , Left 1
    , Left 3
    , Right 1
    , Right 4
    , Right 1
    , Right 3
    , Right 4
    , Right 4
    , Right 4
    , Right 5
    , Right 2
    , Left 5
    , Right 1
    , Right 2
    , Right 5
    , Left 3
    , Left 4
    , Right 1
    , Left 5
    , Right 1
    , Left 4
    , Left 3
    , Right 5
    , Right 5
    , Left 3
    , Left 4
    , Left 4
    , Right 2
    , Right 2
    , Left 5
    , Right 3
    , Right 1
    , Right 2
    , Right 5
    , Left 5
    , Left 3
    , Right 4
    , Left 5
    , Right 5
    , Left 3
    , Right 1
    , Left 1
    , Right 4
    , Right 4
    , Left 3
    , Right 2
    , Right 5
    , Right 1
    , Right 2
    , Left 1
    , Right 4
    , Right 1
    , Left 3
    , Left 3
    , Left 5
    , Right 2
    , Right 5
    , Left 1
    , Left 4
    , Right 3
    , Right 3
    , Left 3
    , Right 2
    , Left 5
    , Right 1
    , Right 3
    , Left 3
    , Right 2
    , Left 1
    , Right 4
    , Right 3
    , Left 4
    , Right 5
    , Left 2
    , Left 2
    , Right 5
    , Right 1
    , Right 2
    , Left 4
    , Left 4
    , Left 5
    , Right 3
    , Left 4
    ]
