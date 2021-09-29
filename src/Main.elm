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
    , loadIntoMemory
    , defaultCpu
    , emptyBuffer
    , updateTimers
    , doNextOp
    , insertRnd
    , endWait
    )
import Random
import Msg exposing ( Msg(..) )
import Html.Attributes exposing (selected)


---- MODEL ----


type alias Model =
    { cpu : Cpu
    , screen : Array Byte
    , run : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        rom = 
            Dict.get "welcome" roms
            |> Maybe.withDefault []
    in
    
    ( { cpu = loadIntoMemory defaultCpu 0x200 rom
      , screen = emptyBuffer
      , run = True
      }
    , Cmd.none
    )


---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    
    -- emulator handlers

    UpdateScreen ->
        ( { model
          | screen = model.cpu.screenBuffer
          , cpu = updateTimers model.cpu
          }, Cmd.none )

    Tick ->
        doNextOp model.cpu
        |> \(cpu, chipMsg) ->
                update chipMsg { model | cpu = cpu }

    SetEmulatorRun b ->
        ( {model | run = b}
        , Cmd.none
        )

    LoadRom name ->
        case Dict.get name roms of
            Just rom ->
                ( { cpu = loadIntoMemory defaultCpu 0x200 rom
                , screen = emptyBuffer
                , run = False
                }
                , Cmd.none
                )
            Nothing ->
                update ( SetEmulatorRun False) model

    -- emulator op runners

    Continue ->
        ( model
        , Cmd.none 
        )

    FetchRandom a b ->
        ( model
        , Random.generate SetRandom (Random.int a b)
        )
    
    SetRandom rnd ->
        ( { model | cpu = insertRnd rnd model.cpu }
        , Cmd.none
        )

    -- input handlers

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

    _ ->
        (model, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view { screen, run } =
    main_ [ class "container" ]
        -- nav bar
        [ nav []
            [ ul []
                [ li []
                    [ h1 [][ strong [][ text "Chip-8"] ] ]
                ]
            , ul []
                [ li [][ button [ class "outline", E.onClick <| SetEmulatorRun (not run) ][ text <| if run then "stop" else "start"] ]
                , li []
                    [ select [ E.onInput LoadRom ]
                        (
                            Dict.keys roms
                            |> List.map
                                (\k ->
                                    option [ selected ( k == "welcome") ][ text k ]
                                )
                        )
                    ]
                ]
            ]

            -- emulator
            , div [ class "emulator" ]
            [ div [ class "screen" ]
                [ render screen ]

            -- inputs
            , div [ class "inputs" ]
                (
                    [(1, "1"),(2, "2"),(3, "3"),(0xA, "A")
                    ,(4, "4"),(5, "5"),(6, "6"),(0xB, "B")
                    ,(7, "7"),(8, "8"),(9, "9"),(0xC, "C")
                    ,(0xD, "D"),(0, "0"),(0xE, "E"),(0xF, "F")
                    ]
                    |> List.map
                        (\(v, t) ->
                            button 
                                [ class "secondary"
                                , E.onMouseDown (InputPressed v)
                                , E.onMouseUp (InputReleased v)
                                ]
                                [text t]
                        )
                )
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