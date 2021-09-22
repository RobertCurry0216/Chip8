module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Time
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array exposing (Array)
import Roms exposing (..)
import Browser.Events
import Json.Decode as Decode
import Dict
import Chip8 exposing (Cpu, Byte, loadIntoMemory, defaultCpu, emptyBuffer, updateTimers, doNextOp)
import Chip8 exposing (ChipMsg(..))


---- MODEL ----


type alias Model =
    { cpu : Cpu
    , screen : Array Byte
    }


init : ( Model, Cmd Msg )
init =
    ( { cpu = loadIntoMemory defaultCpu 0x200 pongRom
      , screen = emptyBuffer
      }
    , Cmd.none
    )


---- UPDATE ----


type Msg
    = UpdateScreen
    | ChipMsg ChipMsg
    | KeyPressed Char
    | KeyReleased Char
    | FetchRandom
    | Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    UpdateScreen ->
        ( { model
          | screen = model.cpu.screenBuffer
          , cpu = updateTimers model.cpu
          }, Cmd.none )
    ChipMsg _ ->
        doNextOp model.cpu
        |> (\(chipMsg, cpu) ->
            case chipMsg of
            InsertRandomInt ->
                ({model | cpu = cpu}, Cmd.none)
            Continue ->
                ({model | cpu = cpu}, Cmd.none)
        )
    KeyPressed c ->
        (   { model 
            | cpu = handleKeyDown model.cpu c
            }
        ,   Cmd.none
        )
    KeyReleased c ->
        (   { model 
            | cpu = handleKeyUp model.cpu c
            }
        ,   Cmd.none
        )
    _ ->
        (model, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view { screen } =
    div []
        [ render screen
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
    [ Time.every (1000 / 60) (\_ -> UpdateScreen)
    , Time.every (1000 / 400) (\_ -> ChipMsg Continue)
    , Browser.Events.onKeyDown (keyEvent KeyPressed)
    , Browser.Events.onKeyUp (keyEvent KeyReleased)
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


handleKeyDown : Cpu -> Char -> Cpu
handleKeyDown cpuIn c =
    case Dict.get c keyMap of
    Just i ->
        { cpuIn
        | keys = Array.set i True cpuIn.keys
        , wait = False
        }
    _ ->
        cpuIn


handleKeyUp : Cpu -> Char -> Cpu
handleKeyUp cpuIn c =
    case Dict.get c keyMap of
    Just i ->
        { cpuIn
        | keys = Array.set i False cpuIn.keys
        }
    _ ->
        cpuIn

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