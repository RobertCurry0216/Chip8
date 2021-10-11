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
import Html
    exposing
        ( Html
        , button
        , div
        , li
        , main_
        , nav
        , strong
        , ul
        , br
        , span
        , fieldset
        )

import Html.Attributes as A
import Html.Attributes exposing (attribute)
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

port playSound : () -> Cmd msg


---- MODEL ----


type alias Model =
    { cpu : Cpu
    , screen : Array Byte
    , run : Bool
    , roms : List String
    , audioOn : Bool
    }


init : List String -> ( Model, Cmd Msg )
init roms =
    ( { cpu = defaultCpu
      , screen = defaultScreen
      , run = False
      , roms = roms
      , audioOn = True
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
            , if model.cpu.timerSound > 0 && model.audioOn then
                playSound ()
              else
                Cmd.none
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

        ToggleSound ->
            ( { model | audioOn = not model.audioOn}
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
            ( { model
                | cpu = loadIntoMemory defaultCpu 0x0200 rom
                , screen = emptyBuffer
                , run = True
                }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { screen, run, roms, audioOn } =
    main_ [ class "" ]
        -- nav bar
        [ nav [ class "tui-nav"]
            [ span [ class "tui-datetime", A.attribute "data-format" "h:m:s a"][]
            , ul []
                [ li []
                    [ span [ class "red-168-text" ] [ strong [] [ text "Chip-8" ] ] ]
               
                ,  li [ class "tui-dropdown" ]
                        [ span [][ text "games"]
                        , div [ class "tui-dropdown-content"]
                            [ fieldset [ class "tui-fieldset"]
                                ( roms
                                    |> List.map
                                        (\k ->
                                            Html.a [ E.onClick <| FetchRom k][ text k]
                                        )
                                )
                            ]
                        ]

                , li []
                    [ Html.a [ E.onClick <| SetEmulatorRun (not run) ]
                        [ text <|
                            if run then
                                "stop"

                            else
                                "start"
                        ]
                    ]

                , li []
                    [ Html.a 
                        [ class <| if audioOn then "" else "secondary"
                        , A.style "text-decoration"
                            <| if audioOn then "none" else "line-through" 
                        , E.onClick
                            <| ToggleSound
                        ]
                        [ text "audio" ]
                    ]
                

                , li [ class "tui-dropdown"]
                    [ span [][ text "keyboard" ]
                    , div [ class "tui-dropdown-content"]
                        [ fieldset [ class "tui-fieldset kbd"]
                            [ text "keyboard mapping"
                            , div [ class "tui-divider"][]
                            , br [][]
                            , text "|1|2|3|C|  =>  |1|2|3|4|"
                            , br [][]
                            , text "|4|5|6|D|  =>  |Q|W|E|R|"
                            , br [][]
                            , text "|7|8|9|E|  =>  |A|S|D|F|"
                            , br [][]
                            , text "|A|0|B|F|  =>  |Z|X|C|V|"
                            ]
                        ]
                    ]
                ]
            ]

        , div [class "container"]
            [
            -- emulator
            div [ class "emulator" ]
                [ div [class "tui-window white-168" ]
                    [ fieldset [ class "tui-fieldset"]
                        [ Html.legend [ A.align "center"][ text "emulator"]
                        , render screen 
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
                                            [ class "tui-button"
                                            , E.onMouseDown (InputPressed v)
                                            , E.onMouseUp (InputReleased v)
                                            , E.on "touchstart" <| Decode.succeed (InputPressed v)
                                            , E.on "touchend" <| Decode.succeed (InputReleased v)
                                            ]
                                            [ text t ]
                                    )
                            )
                        ]
                    ]
                ]
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



-- |1|2|3|C|  =>  |1|2|3|4|
-- |4|5|6|D|  =>  |Q|W|E|R|
-- |7|8|9|E|  =>  |A|S|D|F|
-- |A|0|B|F|  =>  |Z|X|C|V|
keyMap : Dict.Dict Char Int
keyMap =
    Dict.fromList
        [ ( '1', 1 )
        , ( '2', 2 )
        , ( '3', 3 )
        , ( '4', 0xC )
        , ( 'q', 4 )
        , ( 'w', 5 )
        , ( 'e', 6 )
        , ( 'r', 0xD )
        , ( 'a', 7 )
        , ( 's', 8 )
        , ( 'd', 9 )
        , ( 'f', 0xE )
        , ( 'z', 0xA )
        , ( 'x', 0 )
        , ( 'c', 0xB )
        , ( 'v', 0xF )
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


main : Program (List String) Model Msg
main =
    Browser.element
        { view = view
        , init = init
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
