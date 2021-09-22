module Debugging exposing (..)

import Bitwise exposing (and, shiftRightBy)
import Array

intToHex : Int -> String
intToHex int =
    let
        hex = 
            ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F"]
            |> Array.fromList
        
        v =
            and int 0xF

        rest =
            shiftRightBy 4 int

        char =
            Array.get v hex
            |> Maybe.withDefault "x"
    in
    if int >= 16 then
        (intToHex rest) ++ char
    else
        char