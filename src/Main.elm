module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Time
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Chip8 exposing (..)
import Array exposing (Array)
import Bitwise


---- MODEL ----


type alias Model =
    { cpu : Cpu
    }


init : ( Model, Cmd Msg )
init =
    ( { cpu = defaultCpu }
    , Cmd.none 
    )



---- UPDATE ----


type Msg
    = NoOp
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    Tick _ ->
        ( model, Cmd.none )
    _ -> ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ render model.cpu.screenBuffer
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 / toFloat fps) Tick


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


render : Array Byte8 -> Html msg
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