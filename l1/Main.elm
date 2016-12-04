module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Tuple exposing (first, second)
import Array


main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type CardinalDirection
    = North
    | South
    | East
    | West


type Direction
    = Right
    | Left


type alias Point =
    ( Int, Int )


type alias Instruction =
    ( Direction, Int )


type alias CardinalInstruction =
    ( CardinalDirection, Int )


type alias Model =
    { answer1 : Point
    , answer2 : Point
    }


type Msg
    = NoOp
    | Travel


origin : Point
origin =
    ( 0, 0 )


model : Model
model =
    Model origin origin


(>>>) : (a -> b -> c) -> (b -> c -> d) -> a -> b -> d
(>>>) fx gx a b =
    (fx a b) |> (gx b)


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Travel ->
            let
                lastPoint =
                    List.scanl translateInstruction ( North, 0 ) instructions
                        |> List.foldl (move >>> (++)) [ origin ]
                        |> lastItem

                firstIntersection =
                    List.scanl translateInstruction ( North, 0 ) instructions
                        |> List.foldl (move >>> (++)) [ origin ]
                        |> findIntersection
            in
                case ( lastPoint, firstIntersection ) of
                    ( Just a1, Just a2 ) ->
                        { model | answer1 = a1, answer2 = a2 }

                    ( Just a1, Nothing ) ->
                        { model | answer1 = a1 }

                    ( Nothing, Just a2 ) ->
                        { model | answer2 = a2 }

                    ( Nothing, Nothing ) ->
                        model


findIntersection : List Point -> Maybe Point
findIntersection list =
    case list of
        x :: xs ->
            if List.member x xs then
                Just x
            else
                findIntersection xs

        [] ->
            Nothing


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Travel ] [ text "Travel" ]
        , showPoint model.answer1
        , showPoint model.answer2
        ]


showPoint : Point -> Html Msg
showPoint point =
    div []
        [ div [] [ text ("X: " ++ (toString <| Tuple.first point)) ]
        , div [] [ text ("Y: " ++ (toString <| Tuple.second point)) ]
        , div [] [ text ("Distance: " ++ (toString <| distance point)) ]
        ]


distance : Point -> Int
distance point =
    let
        ( x, y ) =
            point
    in
        (abs x) + (abs y)


lastItem : List a -> Maybe a
lastItem list =
    List.foldl (\x _ -> Just x) Nothing list


move : CardinalInstruction -> List Point -> List Point
move instruction points =
    case lastItem points of
        Just x ->
            generatePoints instruction x

        Nothing ->
            []


generatePoints : CardinalInstruction -> Point -> List Point
generatePoints instruction start =
    let
        newInstruction =
            Tuple.mapSecond ((+) -1) instruction

        newStart =
            case (Tuple.first instruction) of
                North ->
                    Tuple.mapSecond ((+) 1) start

                East ->
                    Tuple.mapFirst ((+) 1) start

                South ->
                    Tuple.mapSecond ((+) -1) start

                West ->
                    Tuple.mapFirst ((+) -1) start
    in
        if (Tuple.second newInstruction) >= 0 then
            newStart :: generatePoints newInstruction newStart
        else
            []


translateInstruction : Instruction -> CardinalInstruction -> CardinalInstruction
translateInstruction instruction priorCardinalInstruction =
    Tuple.mapFirst (turn <| Tuple.first priorCardinalInstruction) instruction


turn : CardinalDirection -> Direction -> CardinalDirection
turn facing direction =
    let
        compass =
            [ North, East, South, West ]

        indexOfFacing =
            case getIndex facing compass of
                Just x ->
                    x

                Nothing ->
                    Debug.crash ""

        newIndex =
            case direction of
                Right ->
                    (indexOfFacing + 1) % 4

                Left ->
                    (indexOfFacing - 1) % 4

        newDirection =
            case Array.get newIndex (Array.fromList compass) of
                Just x ->
                    x

                Nothing ->
                    Debug.crash ""
    in
        newDirection


getIndex : a -> List a -> Maybe Int
getIndex e list =
    case list of
        x :: xs ->
            if x == e then
                Just 0
            else
                case getIndex e xs of
                    Just n ->
                        Just (n + 1)

                    Nothing ->
                        Nothing

        [] ->
            Nothing


instructions : List ( Direction, Int )
instructions =
    [ ( Left, 4 )
    , ( Left, 3 )
    , ( Right, 1 )
    , ( Left, 4 )
    , ( Right, 2 )
    , ( Right, 2 )
    , ( Left, 1 )
    , ( Left, 2 )
    , ( Right, 1 )
    , ( Right, 1 )
    , ( Left, 3 )
    , ( Right, 5 )
    , ( Left, 2 )
    , ( Right, 5 )
    , ( Left, 4 )
    , ( Left, 3 )
    , ( Right, 2 )
    , ( Right, 2 )
    , ( Left, 5 )
    , ( Left, 1 )
    , ( Right, 4 )
    , ( Left, 1 )
    , ( Right, 3 )
    , ( Left, 3 )
    , ( Right, 5 )
    , ( Right, 2 )
    , ( Left, 5 )
    , ( Right, 2 )
    , ( Right, 1 )
    , ( Right, 1 )
    , ( Left, 5 )
    , ( Right, 1 )
    , ( Left, 3 )
    , ( Left, 2 )
    , ( Left, 5 )
    , ( Right, 4 )
    , ( Right, 4 )
    , ( Left, 2 )
    , ( Left, 1 )
    , ( Left, 1 )
    , ( Right, 1 )
    , ( Right, 1 )
    , ( Left, 185 )
    , ( Right, 4 )
    , ( Left, 1 )
    , ( Left, 1 )
    , ( Right, 5 )
    , ( Right, 1 )
    , ( Left, 1 )
    , ( Left, 3 )
    , ( Left, 2 )
    , ( Left, 1 )
    , ( Right, 2 )
    , ( Right, 2 )
    , ( Right, 2 )
    , ( Left, 1 )
    , ( Left, 1 )
    , ( Right, 4 )
    , ( Right, 5 )
    , ( Right, 53 )
    , ( Left, 1 )
    , ( Right, 1 )
    , ( Right, 78 )
    , ( Right, 3 )
    , ( Right, 4 )
    , ( Left, 1 )
    , ( Right, 5 )
    , ( Left, 1 )
    , ( Left, 4 )
    , ( Right, 3 )
    , ( Right, 3 )
    , ( Left, 3 )
    , ( Left, 3 )
    , ( Right, 191 )
    , ( Right, 4 )
    , ( Right, 1 )
    , ( Left, 4 )
    , ( Left, 1 )
    , ( Right, 3 )
    , ( Left, 1 )
    , ( Left, 2 )
    , ( Right, 3 )
    , ( Right, 2 )
    , ( Right, 4 )
    , ( Right, 5 )
    , ( Right, 5 )
    , ( Left, 3 )
    , ( Left, 5 )
    , ( Right, 2 )
    , ( Right, 3 )
    , ( Left, 1 )
    , ( Left, 1 )
    , ( Left, 3 )
    , ( Right, 1 )
    , ( Right, 4 )
    , ( Right, 1 )
    , ( Right, 3 )
    , ( Right, 4 )
    , ( Right, 4 )
    , ( Right, 4 )
    , ( Right, 5 )
    , ( Right, 2 )
    , ( Left, 5 )
    , ( Right, 1 )
    , ( Right, 2 )
    , ( Right, 5 )
    , ( Left, 3 )
    , ( Left, 4 )
    , ( Right, 1 )
    , ( Left, 5 )
    , ( Right, 1 )
    , ( Left, 4 )
    , ( Left, 3 )
    , ( Right, 5 )
    , ( Right, 5 )
    , ( Left, 3 )
    , ( Left, 4 )
    , ( Left, 4 )
    , ( Right, 2 )
    , ( Right, 2 )
    , ( Left, 5 )
    , ( Right, 3 )
    , ( Right, 1 )
    , ( Right, 2 )
    , ( Right, 5 )
    , ( Left, 5 )
    , ( Left, 3 )
    , ( Right, 4 )
    , ( Left, 5 )
    , ( Right, 5 )
    , ( Left, 3 )
    , ( Right, 1 )
    , ( Left, 1 )
    , ( Right, 4 )
    , ( Right, 4 )
    , ( Left, 3 )
    , ( Right, 2 )
    , ( Right, 5 )
    , ( Right, 1 )
    , ( Right, 2 )
    , ( Left, 1 )
    , ( Right, 4 )
    , ( Right, 1 )
    , ( Left, 3 )
    , ( Left, 3 )
    , ( Left, 5 )
    , ( Right, 2 )
    , ( Right, 5 )
    , ( Left, 1 )
    , ( Left, 4 )
    , ( Right, 3 )
    , ( Right, 3 )
    , ( Left, 3 )
    , ( Right, 2 )
    , ( Left, 5 )
    , ( Right, 1 )
    , ( Right, 3 )
    , ( Left, 3 )
    , ( Right, 2 )
    , ( Left, 1 )
    , ( Right, 4 )
    , ( Right, 3 )
    , ( Left, 4 )
    , ( Right, 5 )
    , ( Left, 2 )
    , ( Left, 2 )
    , ( Right, 5 )
    , ( Right, 1 )
    , ( Right, 2 )
    , ( Left, 4 )
    , ( Left, 4 )
    , ( Left, 5 )
    , ( Right, 3 )
    , ( Left, 4 )
    ]
