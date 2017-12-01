module Keypad exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array


type alias Model =
    { answer1 : String
    , answer2 : String
    }


type alias Key =
    String


type alias Position =
    ( Int, Int )


type alias Keypad =
    Array.Array (Array.Array (Maybe Key))


type Direction
    = Up
    | Down
    | Left
    | Right


normalKeypad : Keypad
normalKeypad =
    Array.fromList
        [ Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing ]
        , Array.fromList [ Nothing, Just "1", Just "2", Just "3", Nothing ]
        , Array.fromList [ Nothing, Just "4", Just "5", Just "6", Nothing ]
        , Array.fromList [ Nothing, Just "7", Just "8", Just "9", Nothing ]
        , Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing ]
        ]


advancedKeypad : Keypad
advancedKeypad =
    Array.fromList
        [ Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
        , Array.fromList [ Nothing, Nothing, Nothing, Just "1", Nothing, Nothing, Nothing ]
        , Array.fromList [ Nothing, Nothing, Just "2", Just "3", Just "4", Nothing, Nothing ]
        , Array.fromList [ Nothing, Just "5", Just "6", Just "7", Just "8", Just "9", Nothing ]
        , Array.fromList [ Nothing, Nothing, Just "A", Just "B", Just "C", Nothing, Nothing ]
        , Array.fromList [ Nothing, Nothing, Nothing, Just "D", Nothing, Nothing, Nothing ]
        , Array.fromList [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
        ]


model : Model
model =
    { answer1 = ""
    , answer2 = ""
    }


parseDirection : Char -> Maybe Direction
parseDirection c =
    case c of
        'U' ->
            Just Up

        'R' ->
            Just Right

        'D' ->
            Just Down

        'L' ->
            Just Left

        _ ->
            Nothing


parseDirections : String -> List Direction
parseDirections =
    String.toList >> List.filterMap parseDirection


parseCode : List String -> Parser
parseCode =
    List.map parseDirections


type alias Parser =
    List (List Direction)


type alias Decoder =
    List Direction -> Position -> Position


type alias Translator =
    List Position -> String


solve : Decoder -> Position -> Parser -> List Position
solve =
    List.scanl


resultsTranslator : Keypad -> Translator
resultsTranslator keypad =
    List.map (getKey keypad)
        >> List.filterMap identity
        >> List.tail
        >> answerToString


solveDigit : Keypad -> Decoder
solveDigit keypad directions startPosition =
    List.foldl (movePosition keypad) startPosition directions


movePosition : Keypad -> Direction -> Position -> Position
movePosition keypad direction position =
    let
        nextPosition =
            getNextPosition position direction
    in
        case getKey keypad nextPosition of
            Just key ->
                nextPosition

            Nothing ->
                position


getNextPosition : Position -> Direction -> Position
getNextPosition position direction =
    case direction of
        Up ->
            Tuple.mapSecond ((+) -1) position

        Right ->
            Tuple.mapFirst ((+) 1) position

        Down ->
            Tuple.mapSecond ((+) 1) position

        Left ->
            Tuple.mapFirst ((+) -1) position


getKey : Keypad -> Position -> Maybe Key
getKey keypad position =
    case Array.get (Tuple.second position) keypad of
        Just row ->
            case Array.get (Tuple.first position) row of
                Just key ->
                    key

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


type Msg
    = NoOp
    | Solve


answerToString : Maybe (List String) -> String
answerToString answer =
    case answer of
        Just ans ->
            String.concat ans

        Nothing ->
            "No Answer"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Solve ->
            let
                answer1 =
                    solve
                        (solveDigit normalKeypad)
                        ( 2, 2 )
                        (parseCode actualCode)
                        |> (resultsTranslator normalKeypad)

                answer2 =
                    solve
                        (solveDigit advancedKeypad)
                        ( 1, 3 )
                        (parseCode actualCode)
                        |> (resultsTranslator advancedKeypad)
            in
                ( { model | answer1 = answer1, answer2 = answer2 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick Solve ] [ text "Solve" ]
            ]
        , div [] [ text ("Answer 1: " ++ model.answer1) ]
        , div [] [ text ("Answer 2: " ++ model.answer2) ]
        ]


viewCode : List String -> Html Msg
viewCode code =
    let
        codeItems =
            List.map (\x -> li [] [ text x ]) code
    in
        ul [] codeItems


testCode : List String
testCode =
    [ "ULL"
    , "RRDDD"
    , "LURDL"
    , "UUUD"
    ]


actualCode : List String
actualCode =
    [ "DLRURUDLULRDRUDDRLUUUDLDLDLRLRRDRRRLLLLLDDRRRDRRDRRRLRRURLRDUULRLRRDDLULRLLDUDLULURRLRLDUDLURURLDRDDULDRDRDLDLLULULLDDLRRUDULLUULRRLLLURDRLDDLDDLDRLRRLLRURRUURRRRLUDLRDDDDRDULRLLDDUURDUDRLUDULLUDLUDURRDRDUUUUDDUDLLLRLUULRUURDLRLLRRLRLLDLLRLLRRRURLRRLURRLDLLLUUDURUDDLLUURRDRDRRDLLDDLLRDRDRRLURLDLDRDLURLDULDRURRRUDLLULDUDRURULDUDLULULRRRUDLUURRDURRURRLRRLLRDDUUUUUDUULDRLDLLRRUDRRDULLLDUDDUDUURLRDLULUUDLDRDUUUDDDUDLDURRULUULUUULDRUDDLLLDLULLRLRLUDULLDLLRLDLDDDUUDURDDDLURDRRDDLDRLLRLRR"
    , "RLDUDURDRLLLLDDRRRURLLLRUUDDLRDRDDDUDLLUDDLRDURLDRDLLDRULDDRLDDDRLDRDDDRLLULDURRRLULDRLRDRDURURRDUDRURLDRLURDRLUULLULLDLUDUDRDRDDLDDDDRDURDLUDRDRURUDDLLLRLDDRURLLUDULULDDLLLDLUDLDULUUDLRLURLDRLURURRDUUDLRDDDDDRLDULUDLDDURDLURLUURDLURLDRURRLDLLRRUDRUULLRLDUUDURRLDURRLRUULDDLDLDUUDDRLDLLRRRUURLLUURURRURRLLLUDLDRRDLUULULUDDULLUDRLDDRURDRDUDULUDRLRRRUULLDRDRLULLLDURURURLURDLRRLLLDRLDUDLLLLDUUURULDDLDLLRRUDDDURULRLLUDLRDLUUDDRDDLLLRLUURLDLRUURDURDDDLLLLLULRRRURRDLUDLUURRDRLRUDUUUURRURLRDRRLRDRDULLDRDRLDURDDUURLRUDDDDDLRLLRUDDDDDURURRLDRRUUUDLURUUDRRDLLULDRRLRRRLUUUD"
    , "RDRURLLUUDURURDUUULLRDRLRRLRUDDUDRURLLDLUUDLRLLDDURRURLUDUDDURLURLRRURLLURRUDRUDLDRLLURLRUUURRUDDDURRRLULLLLURDLRLLDDRLDRLLRRDLURDLRDLDUDRUULLDUUUDLURRLLRUDDDUUURLURUUDRLRULUURLLRLUDDLLDURULLLDURDLULDLDDUDULUDDULLRDRURDRRLLDLDDDDRUDLDRRLLLRLLLRRULDLRLRLRLLDLRDRDLLUDRDRULDUURRDDDRLLRLDLDRDUDRULUDRDLDLDDLLRULURLLURDLRRDUDLULLDLULLUDRRDDRLRURRLDUDLRRUUDLDRLRLDRLRRDURRDRRDDULURUUDDUUULRLDRLLDURRDLUULLUDRDDDLRUDLRULLDDDLURLURLRDRLLURRRUDLRRLURDUUDRLRUUDUULLRUUUDUUDDUURULDLDLURLRURLRUDLULLULRULDRDRLLLRRDLU"
    , "RRRRDRLUUULLLRLDDLULRUUURRDRDRURRUURUDUULRULULRDRLRRLURDRRRULUUULRRUUULULRDDLLUURRLLDUDRLRRLDDLDLLDURLLUDLDDRRURLDLULRDUULDRLRDLLDLRULLRULLUDUDUDDUULDLUUDDLUDDUULLLLLURRDRULURDUUUDULRUDLLRUUULLUULLLRUUDDRRLRDUDDRULRDLDLLLLRLDDRRRULULLLDLRLURRDULRDRDUDDRLRLDRRDLRRRLLDLLDULLUDDUDDRULLLUDDRLLRRRLDRRURUUURRDLDLURRDLURULULRDUURLLULDULDUDLLULDDUURRRLDURDLUDURLDDRDUDDLLUULDRRLDLLUDRDURLLDRLDDUDURDLUUUUURRUULULLURLDUUULLRURLLLUURDULLUULDRULLUULRDRUULLRUDLDDLRLURRUUDRLRRRULRUUULRULRRLDLUDRRLL"
    , "ULRLDLLURDRRUULRDUDDURDDDLRRRURLDRUDDLUDDDLLLRDLRLLRRUUDRRDRUULLLULULUUDRRRDRDRUUUUULRURUULULLULDULURRLURUDRDRUDRURURUDLDURUDUDDDRLRLLLLURULUDLRLDDLRUDDUUDURUULRLLLDDLLLLRRRDDLRLUDDUULRRLLRDUDLLDLRRUUULRLRDLRDUDLLLDLRULDRURDLLULLLRRRURDLLUURUDDURLDUUDLLDDRUUDULDRDRDRDDUDURLRRRRUDURLRRUDUDUURDRDULRLRLLRLUDLURUDRUDLULLULRLLULRUDDURUURDLRUULDURDRRRLLLLLUUUULUULDLDULLRURLUDLDRLRLRLRDLDRUDULDDRRDURDDULRULDRLRULDRLDLLUDLDRLRLRUDRDDR"
    ]
