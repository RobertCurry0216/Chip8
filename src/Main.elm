module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Time
import Svg exposing (..)
import Svg.Attributes exposing (..)


---- MODEL ----


type alias Model =
    { screen : List Bool
    , pointer : Int
    }


init : ( Model, Cmd Msg )
init =
    (   { screen = List.repeat (64*32) True
        , pointer = 0
    }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
    Tick _ ->
        model.screen
        |> List.indexedMap (\i _ -> i == model.pointer)
        |> (\s -> ({model | screen = s, pointer = model.pointer+1}, Cmd.none))
    _ -> ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ render model.screen
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Tick


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



render : List Bool -> Html msg
render screen =
    screen
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