port module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events
import Chip8
    exposing
        ( Byte
        , Cpu
        , defaultCpu
        , doNextOp
        , emptyBuffer
        , endWait
        , insertRnd
        , loadIntoMemory
        , updateTimers
        , defaultScreen
        )
import Dict
import Html
    exposing
        ( Html
        , button
        , div
        , h1
        , li
        , main_
        , nav
        , option
        , select
        , strong
        , ul
        )
import Html.Attributes exposing (selected)
import Html.Events as E
import Json.Decode as Decode
import Msg exposing (Msg(..))
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


---- ports ----

port fetchRom : String -> Cmd msg

port loadRom : (List Int -> msg) -> Sub msg


---- MODEL ----


type alias Model =
    { cpu : Cpu
    , screen : Array Byte
    , run : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { cpu = defaultCpu
      , screen = defaultScreen
      , run = False
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
              }
            , Cmd.none
            )

        Tick ->
            doNextOp model.cpu
                |> (\( cpu, chipMsg ) ->
                        update chipMsg { model | cpu = cpu }
                   )

        SetEmulatorRun b ->
            ( { model | run = b }
            , Cmd.none
            )

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
            ( { model
                | cpu = handleInputDown model.cpu c
              }
            , Cmd.none
            )

        InputReleased c ->
            ( { model
                | cpu = handleInputUp model.cpu c
              }
            , Cmd.none
            )

        -- interop
        FetchRom name ->
            ( model
            , fetchRom name
            )

        LoadRom rom ->
            ( { cpu = loadIntoMemory defaultCpu 0x0200 rom
                , screen = emptyBuffer
                , run = True
                }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { screen, run } =
    main_ [ class "container" ]
        -- nav bar
        [ nav []
            [ ul []
                [ li []
                    [ h1 [] [ strong [] [ text "Chip-8" ] ] ]
                ]
            , ul []
                [ li []
                    --[ button [ class "outline", E.onClick <| SetEmulatorRun (not run) ]
                    [ button [ class "outline", E.onClick <| FetchRom "Missile" ]
                        [ text <|
                            if run then
                                "stop"

                            else
                                "start"
                        ]
                    ]
                , li []
                    [ select [ E.onInput (\_ -> Noop) ]
                        (Dict.keys Dict.empty
                            |> List.map
                                (\k ->
                                    option [ selected (k == "welcome") ] [ text k ]
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
                ([ ( 1, "1" )
                 , ( 2, "2" )
                 , ( 3, "3" )
                 , ( 0x0C, "C" )
                 , ( 4, "4" )
                 , ( 5, "5" )
                 , ( 6, "6" )
                 , ( 0x0D, "D" )
                 , ( 7, "7" )
                 , ( 8, "8" )
                 , ( 9, "9" )
                 , ( 0x0E, "E" )
                 , ( 0x0A, "A" )
                 , ( 0, "0" )
                 , ( 0x0B, "B" )
                 , ( 0x0F, "F" )
                 ]
                    |> List.map
                        (\( v, t ) ->
                            button
                                [ class "secondary"
                                , E.onMouseDown (InputPressed v)
                                , E.onMouseUp (InputReleased v)
                                ]
                                [ text t ]
                        )
                )
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.run then
            Time.every (1000 / 60) (\_ -> UpdateScreen)

          else
            Sub.none
        , if model.run then
            Time.every (1000 / 400) (\_ -> Tick)

          else
            Sub.none

        , Browser.Events.onKeyDown (keyEvent keyPressed)
        , Browser.Events.onKeyUp (keyEvent keyReleased)

        , loadRom LoadRom
        ]



---- INPUT ----


toKey : (Char -> Msg) -> String -> Msg
toKey msg string =
    case String.uncons string of
        Just ( char, "" ) ->
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
        [ ( 'q', 0 )
        , ( 'w', 1 )
        , ( 'e', 2 )
        , ( 'a', 3 )
        , ( 's', 4 )
        , ( 'd', 5 )
        , ( 'z', 6 )
        , ( 'x', 7 )
        , ( 'c', 8 )
        , ( 'r', 9 )
        , ( 'f', 10 )
        , ( 'v', 11 )
        , ( 't', 12 )
        , ( 'g', 13 )
        , ( 'b', 14 )
        , ( ' ', 15 )
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


getCoords : Int -> ( Int, Int )
getCoords idx =
    let
        x =
            Basics.modBy 64 idx

        y =
            idx // 64
    in
    ( x * 10, y * 10 )


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
            (\( a, b ) ->
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
