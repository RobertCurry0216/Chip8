module Main exposing (..)

import Html exposing 
    ( Html
    , div
    , main_
    , button
    , nav
    , ul
    , li
    , strong
    , select
    , option
    , h1
    )
import Html.Events as E
import Browser
import Time
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array exposing (Array)
import Roms exposing (..)
import Browser.Events
import Json.Decode as Decode
import Dict
import Chip8 exposing 
    ( Cpu
    , Byte
    , ChipMsg(..)
    , loadIntoMemory
    , defaultCpu
    , emptyBuffer
    , updateTimers
    , doNextOp
    , insertRnd
    , endWait
    )
import Random


---- MODEL ----


type alias Model =
    { cpu : Cpu
    , screen : Array Byte
    , run : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { cpu = loadIntoMemory defaultCpu 0x200 tetrisRom
      , screen = emptyBuffer
      , run = False
      }
    , Cmd.none
    )


---- UPDATE ----


type Msg
    = UpdateScreen
    | Tick
    | InputPressed Int
    | InputReleased Int
    | SetEmulatorRun Bool
    | SetRandom Int
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    UpdateScreen ->
        ( { model
          | screen = model.cpu.screenBuffer
          , cpu = updateTimers model.cpu
          }, Cmd.none )

    Tick ->
        doNextOp model.cpu
        |> (\(cpu, chipMsg) ->
            let
                newModel =
                   { model
                    | screen = cpu.screenBuffer
                    , cpu = updateTimers cpu
                    } 
            in
            case chipMsg of
            InsertRandomInt a b -> 
                ( newModel, Random.generate SetRandom (Random.int a b) )
            Stop ->
                update ( SetEmulatorRun False ) newModel
            _ -> 
                ( newModel, Cmd.none )
        )

    SetRandom rnd ->
        ( { model | cpu = insertRnd rnd model.cpu }
        , Cmd.none
        )

    InputPressed c ->
        (   { model 
            | cpu = handleInputDown model.cpu c
            }
        ,   Cmd.none
        )

    InputReleased c ->
        (   { model 
            | cpu = handleInputUp model.cpu c
            }
        ,   Cmd.none
        )

    SetEmulatorRun b ->
        ( {model | run = b}, Cmd.none)

    _ ->
        (model, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view { screen, run } =
    main_ [ class "container" ]
        [ nav []
            [ ul []
                [ li []
                    [ h1 [][ strong [][ text "Chip-8"] ] ]
                ]
            , ul []
                [ li [][ button [ class "outline", E.onClick <| SetEmulatorRun (not run) ][ text <| if run then "stop" else "start"] ]
                , li []
                    [ select [ ]
                        [ option [][text "tetris"]
                        , option [][text "pong"]
                        ]
                    ]
                ]
            ]
            , div [ class "emulator" ]
            [ div [ class "screen" ]
                [ render screen ]
            , div [ class "inputs" ]
                [ button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 1)
                    , E.onMouseUp (InputReleased 1) 
                    ]
                    [text "1"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 2)
                    , E.onMouseUp (InputReleased 2) 
                    ]
                    [text "2"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 3)
                    , E.onMouseUp (InputReleased 3) 
                    ]
                    [text "3"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 10)
                    , E.onMouseUp (InputReleased 10) 
                    ]
                    [text "A"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 4)
                    , E.onMouseUp (InputReleased 4) 
                    ]
                    [text "4"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 5)
                    , E.onMouseUp (InputReleased 5) 
                    ]
                    [text "5"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 6)
                    , E.onMouseUp (InputReleased 6) 
                    ]
                    [text "6"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 11)
                    , E.onMouseUp (InputReleased 11) 
                    ]
                    [text "B"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 7)
                    , E.onMouseUp (InputReleased 7) 
                    ]
                    [text "7"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 8)
                    , E.onMouseUp (InputReleased 8) 
                    ]
                    [text "8"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 9)
                    , E.onMouseUp (InputReleased 9) 
                    ]
                    [text "9"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 12)
                    , E.onMouseUp (InputReleased 12) 
                    ]
                    [text "C"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 13)
                    , E.onMouseUp (InputReleased 13) 
                    ]
                    [text "D"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 0)
                    , E.onMouseUp (InputReleased 0) 
                    ]
                    [text "0"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 14)
                    , E.onMouseUp (InputReleased 14) 
                    ]
                    [text "E"]
                , button 
                    [ class "secondary"
                    , E.onMouseDown (InputPressed 15)
                    , E.onMouseUp (InputReleased 15) 
                    ]
                    [text "F"]
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ if model.run then Time.every (1000 / 60) (\_ -> UpdateScreen) else Sub.none
    , if model.run then Time.every (1000 / 400) (\_ -> Tick) else Sub.none
    , Browser.Events.onKeyDown (keyEvent keyPressed)
    , Browser.Events.onKeyUp (keyEvent keyReleased)
    ]


---- INPUT ----

toKey : (Char -> Msg) -> String -> Msg
toKey msg string =
    case String.uncons string of
    Just (char, "") ->
        msg char
    _ -> 
        Noop


keyEvent : (Char -> Msg) -> Decode.Decoder Msg
keyEvent msg =
    Decode.map (toKey msg) (Decode.field "key" Decode.string)


keyPressed : Char -> Msg
keyPressed c =
    case Dict.get c keyMap of
    Just i ->
        InputPressed i
    Nothing ->
        Noop


keyReleased : Char -> Msg
keyReleased c =
    case Dict.get c keyMap of
    Just i ->
        InputReleased i
    Nothing ->
        Noop


keyMap : Dict.Dict Char Int
keyMap =
    Dict.fromList
    [ ('q', 0)
    , ('w', 1)
    , ('e', 2)
    , ('a', 3)
    , ('s', 4)
    , ('d', 5)
    , ('z', 6)
    , ('x', 7)
    , ('c', 8)
    , ('r', 9)
    , ('f', 10)
    , ('v', 11)
    , ('t', 12)
    , ('g', 13)
    , ('b', 14)
    , (' ', 15)
    ]


handleInputDown : Cpu -> Int -> Cpu
handleInputDown cpuIn c =
    { cpuIn
    | keys = Array.set c True cpuIn.keys
    }
    |> endWait c



handleInputUp : Cpu -> Int -> Cpu
handleInputUp cpuIn c =
    { cpuIn
    | keys = Array.set c False cpuIn.keys
    }

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


---- Chip 8 helpers ----

getCoords : Int -> (Int, Int)
getCoords idx =
    let
        x =
            Basics.modBy 64 idx

        y =
            idx // 64
    in
    (x*10, y*10)


render : Array Byte -> Html msg
render screen =
    screen
    |> Array.toList
    |> List.map (\bit -> bit == 1)
    |> List.indexedMap 
        (\i v -> 
            if v then
                Just <| getCoords i
            else 
                Nothing
        )
    |> List.filterMap identity
    |> List.map
        (\(a, b) -> 
            rect
                [ x (String.fromInt a)
                , y (String.fromInt b)
                , width "10"
                , height "10"
                , fill "black"
                , stroke "grey"
                ]
                []
        )
    |> svg
        [ viewBox "0 0 640 320"
        , width "640"
        , height "320"
        ]