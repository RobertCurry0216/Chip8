module Tests exposing (..)

import Test exposing (..)
import Expect
import Chip8 exposing (..)
import Array


-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


opcodes : Test
opcodes =
    describe "Chi8 opcodes"
    [ test "op_00EE: return from a subroutine"
        (\_ ->
            let
                cpu = { defaultCpu | stack = [4, 8, 16] }
            in
            op_00EE 0 cpu
            |> .pc
            |> Expect.equal 4)

    , test "op_1NNN: jump to nnn"
        (\_ ->
            op_1NNN 0x123 defaultCpu
            |> .pc
            |> Expect.equal 0x123
        )

    , test "op_2NNN: call subroutine at nnn"
        (\_ ->
            let
                cpu = { defaultCpu | pc = 123, stack = [1,2,3] }
            in
            op_2NNN 0x2456 cpu
            |> Expect.all
                [ \{ pc } -> Expect.equal 0x456 pc
                , \{ stack } -> Expect.equal [123,1,2,3] stack
                ]
        )

    , describe "op_3XKK: skip if Vx == kk"
        [ test "Vx == kk"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , registers = Array.set 0 0xA emptyRegisters
                            }
                in
                op_3XKK 0x300A cpu
                |> .pc
                |> Expect.equal 102
            )
        , test "Vx /= kk"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , registers = Array.set 0 0xA emptyRegisters
                            }
                in
                op_3XKK 0x300B cpu
                |> .pc
                |> Expect.equal 100
            )

        ]

    , describe "op_4XKK: skip if Vx /= kk"
        [ test "Vx == kk"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , registers = Array.set 0 0xA emptyRegisters
                            }
                in
                op_4XKK 0x400A cpu
                |> .pc
                |> Expect.equal 100
            )
        , test "Vx /= kk"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , registers = Array.set 0 0xA emptyRegisters
                            }
                in
                op_4XKK 0x400B cpu
                |> .pc
                |> Expect.equal 102
            )

        ]

    , describe "op_5XY0: skip if Vx == Vy"
        [ test "Vx == Vy"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , registers = 
                                Array.set 0 0xA emptyRegisters 
                                |> Array.set 1 0xA
                            }
                in
                op_5XY0 0x5010 cpu
                |> .pc
                |> Expect.equal 102
            )
        , test "Vx /= Vy"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , registers = 
                                Array.set 0 0xA emptyRegisters 
                                |> Array.set 1 0xB
                            }
                in
                op_5XY0 0x5010 cpu
                |> .pc
                |> Expect.equal 100
            )

        ]

    , test "op_6XKK: Set Vx to kk"
        (\_ ->
            op_6XKK 0x61AB defaultCpu
            |> .registers
            |> Array.get 1
            |> Expect.equal (Just 0xAB)
        )

    , test "op_7XKK: Set Vx to Vx + kk"
        (\_ ->
            let
                cpu =   { defaultCpu 
                        | registers = Array.set 1 2 emptyRegisters
                        }
            in
            op_7XKK 0x7112 cpu
            |> .registers
            |> Array.get 1
            |> Expect.equal ( Just 20 )
        )

    , test "op_8XY0: Set Vx to Vy"
        (\_ ->
            let
                cpu =   { defaultCpu 
                        | registers = 
                            Array.set 1 0xA emptyRegisters 
                        }
            in
            op_8XY0 0x8010 cpu
            |> .registers
            |> Array.get 0
            |> Expect.equal ( Just 0xA )
        )

    , test "op_8XY1: Set Vx = Vx OR Vy."
        (\_ ->
            let
                cpu =   { defaultCpu 
                        | registers = 
                            Array.set 0 0x1 emptyRegisters 
                            |> Array.set 1 0x2
                        }
            in
            op_8XY1 0x8011 cpu
            |> .registers
            |> Array.get 0
            |> Expect.equal ( Just 0x3 )
        )

    , test "op_8XY2: Set Vx = Vx AND Vy."
        (\_ ->
            let
                cpu =   { defaultCpu 
                        | registers = 
                            Array.set 0 0x5 emptyRegisters 
                            |> Array.set 1 0x3
                        }
            in
            op_8XY2 0x8012 cpu
            |> .registers
            |> Array.get 0
            |> Expect.equal ( Just 0x1 )
        )

    , test "op_8XY3: Set Vx = Vx XOR Vy."
        (\_ ->
            let
                cpu =   { defaultCpu 
                        | registers = 
                            Array.set 0 0x5 emptyRegisters 
                            |> Array.set 1 0x3
                        }
            in
            op_8XY3 0x8013 cpu
            |> .registers
            |> Array.get 0
            |> Expect.equal ( Just 0x6 )
        )

    , describe "op_8XY4: Set Vx = Vx + Vy, set VF = carry"
        [ test "no overflow, no carry"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | registers = 
                                Array.set 0 0x5 emptyRegisters 
                                |> Array.set 1 0x3
                            }
                in
                op_8XY4 0x8014 cpu
                |> .registers
                |> Expect.all
                    [ \r -> Array.get 0 r |> Expect.equal ( Just 0x8 )
                    , \r -> Array.get 0xF r |> Expect.equal ( Just 0x0 )
                    ]
            )
        , test "overflow and carry"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | registers = 
                                Array.set 0 0xFF emptyRegisters 
                                |> Array.set 1 0x2
                            }
                in
                op_8XY4 0x8014 cpu
                |> .registers
                |> Expect.all
                    [ \r -> Array.get 0 r |> Expect.equal ( Just 0x1 )
                    , \r -> Array.get 0xF r |> Expect.equal ( Just 0x1 )
                    ]
            )
        ]

    , describe "op_8XY5: Set Vx = Vx - Vy, set VF = NOT borrow."
        [ test "no overflow, carry"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | registers = 
                                Array.set 0 0x5 emptyRegisters 
                                |> Array.set 1 0x3
                            }
                in
                op_8XY5 0x8015 cpu
                |> .registers
                |> Expect.all
                    [ \r -> Array.get 0 r |> Expect.equal ( Just 0x2 )
                    , \r -> Array.get 0xF r |> Expect.equal ( Just 0x1 )
                    ]
            )
        , test "overflow and not carry"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | registers = 
                                Array.set 0 0x0 emptyRegisters 
                                |> Array.set 1 0x1
                            }
                in
                op_8XY5 0x8015 cpu
                |> .registers
                |> Expect.all
                    [ \r -> Array.get 0 r |> Expect.equal ( Just 0xFF )
                    , \r -> Array.get 0xF r |> Expect.equal ( Just 0x0 )
                    ]
            )
        ]

    , describe "op_8XY6: Set Vx = Vx SHR 1"
        [ test "even"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | registers = 
                                Array.set 0 0x4 emptyRegisters
                            }
                in
                op_8XY6 0x8016 cpu
                |> .registers
                |> Expect.all
                    [ \r -> Array.get 0 r |> Expect.equal ( Just 0x2 )
                    , \r -> Array.get 0xF r |> Expect.equal ( Just 0x0 )
                    ]
            )
        , test "odd"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | registers = 
                                Array.set 0 0x5 emptyRegisters
                            }
                in
                op_8XY6 0x8016 cpu
                |> .registers
                |> Expect.all
                    [ \r -> Array.get 0 r |> Expect.equal ( Just 0x2 )
                    , \r -> Array.get 0xF r |> Expect.equal ( Just 0x1 )
                    ]
            )
        ]
    , describe "op_8XY7: Set Vx = Vy - Vx, set VF = NOT borrow."
        [ test "no overflow, carry"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | registers = 
                                Array.set 0 0x3 emptyRegisters 
                                |> Array.set 1 0x5
                            }
                in
                op_8XY7 0x8017 cpu
                |> .registers
                |> Expect.all
                    [ \r -> Array.get 0 r |> Expect.equal ( Just 0x2 )
                    , \r -> Array.get 0xF r |> Expect.equal ( Just 0x1 )
                    ]
            )
        , test "overflow and not carry"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | registers = 
                                Array.set 0 0x1 emptyRegisters 
                                |> Array.set 1 0x0
                            }
                in
                op_8XY7 0x8017 cpu
                |> .registers
                |> Expect.all
                    [ \r -> Array.get 0 r |> Expect.equal ( Just 0xFF )
                    , \r -> Array.get 0xF r |> Expect.equal ( Just 0x0 )
                    ]
            ) 
        ]

    , describe "op_8XYE: Set Vx = Vx SHL 1"
        [ test "no overflow"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | registers = 
                                Array.set 0 0x4 emptyRegisters
                            }
                in
                op_8XYE 0x801E cpu
                |> .registers
                |> Expect.all
                    [ \r -> Array.get 0 r |> Expect.equal ( Just 0x8 )
                    , \r -> Array.get 0xF r |> Expect.equal ( Just 0x0 )
                    ]
            )
        , test "overflow"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | registers = 
                                -- 0x81 = 0b10000001
                                Array.set 0 0x81 emptyRegisters
                            }
                in
                op_8XYE 0x801E cpu
                |> .registers
                |> Expect.all
                    [ \r -> Array.get 0 r |> Expect.equal ( Just 0x2 )
                    , \r -> Array.get 0xF r |> Expect.equal ( Just 0x1 )
                    ]
            )
        ]

    , describe "op_9XY0: skip if Vx /= Vy"
        [ test "Vx == Vy"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , registers = 
                                Array.set 0 0xA emptyRegisters 
                                |> Array.set 1 0xA
                            }
                in
                op_9XY0 0x9010 cpu
                |> .pc
                |> Expect.equal 100
            )
        , test "Vx /= Vy"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , registers = 
                                Array.set 0 0xA emptyRegisters 
                                |> Array.set 1 0xB
                            }
                in
                op_9XY0 0x9010 cpu
                |> .pc
                |> Expect.equal 102
            )
        ]

    , test "op_ANNN: Set I = nnn"
        (\_ ->
            op_ANNN 0xA123 defaultCpu
            |> .i
            |> Expect.equal 0x123
        )

    , test "op_BNNN: Jump to location nnn + V0"
        (\_ ->
            let
                cpu = 
                    { defaultCpu
                    | registers = Array.set 0 2 emptyRegisters
                    }
            in
            op_BNNN 0xB008 cpu
            |> .pc
            |> Expect.equal 0xA
        )

    , describe "op_CXKK: Set Vx = random byte AND kk"
        [ test "save vx and kk"
            (\_ ->
                op_CXKK 0xC1F0 defaultCpu
                |> .rnd
                |> Expect.equal { vx=0x1, kk=0xF0 }
            )
        , test "insert rnd"
            (\_ ->
                let
                    cpu =
                        { defaultCpu
                        | rnd = { vx=0x1, kk=0xF0 }
                        }
                in
                insertRnd 0xAB cpu
                |> .registers
                |> Array.get 0x1
                |> Expect.equal ( Just 0xA0 )
            )
        ]

    , describe "op_EX9E: skip if key(Vx) is pressed"
        [ test "pressed"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , keys =
                                Array.set 0 True emptyKeys
                            }
                in
                op_EX9E 0x909E cpu
                |> .pc
                |> Expect.equal 102
            )
        , test "not pressed"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , keys =
                                Array.set 0 False emptyKeys
                            }
                in
                op_EX9E 0x909E cpu
                |> .pc
                |> Expect.equal 100
            )
        ]

    , describe "op_EXA1: skip if key(Vx) is not pressed"
        [ test "pressed"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , keys =
                                Array.set 0 True emptyKeys
                            }
                in
                op_EXA1 0x90A1 cpu
                |> .pc
                |> Expect.equal 100
            )
        , test "not pressed"
            (\_ ->
                let
                    cpu =   { defaultCpu 
                            | pc = 100
                            , keys =
                                Array.set 0 False emptyKeys
                            }
                in
                op_EXA1 0x90A1 cpu
                |> .pc
                |> Expect.equal 102
            )
        ]
    , test "op_FX07: Set Vx = delay timer value"
        (\_ -> 
            let
                cpu = 
                    { defaultCpu
                    | timerDelay = 0xA
                    }
            in
            op_FX07 0xF007 cpu
            |> .registers
            |> Array.get 0
            |> Expect.equal ( Just 0xA )
        )

    , test "op_FX0A: Wait for keypress"
        (\_ -> 
            op_FX0A 0xFA0A defaultCpu
            |> Expect.all
                [ \{ wait } -> Expect.equal True wait
                , \{ waitRegister } -> Expect.equal 0xA waitRegister
                ]
        )

    , test "op_FX15: Set delay timer = Vx"
        (\_ ->
            let
                cpu = 
                    { defaultCpu
                    | registers = Array.set 0 0xA emptyRegisters
                    }
            in
            op_FX15 0xF015 cpu
            |> .timerDelay
            |> Expect.equal 0xA
        )

    , test "op_FX18: Set sound timer = Vx"
        (\_ ->
            let
                cpu = 
                    { defaultCpu
                    | registers = Array.set 0 0xA emptyRegisters
                    }
            in
            op_FX18 0xF018 cpu
            |> .timerSound
            |> Expect.equal 0xA
        )
    
    , test "op_FX1E: Set sound timer = Vx"
        (\_ ->
            let
                cpu = 
                    { defaultCpu
                    | registers = Array.set 0 0xA emptyRegisters
                    , i = 0xAA0
                    }
            in
            op_FX1E 0xF01E cpu
            |> .i
            |> Expect.equal 0xAAA
        )

    , test "op_FX33: Store BCD representation of Vx in memory locations I, I+1, and I+2."
        (\_ ->
            let
                cpu = 
                    { defaultCpu
                    | registers = Array.set 0 123 emptyRegisters
                    , i = 0x200
                    }
            in
            op_FX33 0xF033 cpu
            |> .memory
            |> Expect.all
                [ \m -> Array.get 0x200 m |> Expect.equal ( Just 1 )
                , \m -> Array.get 0x201 m |> Expect.equal ( Just 2 )
                , \m -> Array.get 0x202 m |> Expect.equal ( Just 3 )
                ]
        )

    , test "op_FX55: Store registers V0 through Vx in memory starting at location I"
        (\_ ->
            let
                cpu =
                    { defaultCpu
                    | i = 0x200
                    , registers =
                        emptyRegisters
                        |> Array.set 0 0x1
                        |> Array.set 1 0x2
                        |> Array.set 2 0x3
                        |> Array.set 3 0x4
                        |> Array.set 4 0x5
                    }
            in
            op_FX55 0xF455 cpu
            |> .memory
            |> Expect.all
                [ \m -> Array.get 0x200 m |> Expect.equal ( Just 1 )
                , \m -> Array.get 0x201 m |> Expect.equal ( Just 2 )
                , \m -> Array.get 0x202 m |> Expect.equal ( Just 3 )
                , \m -> Array.get 0x203 m |> Expect.equal ( Just 4 )
                , \m -> Array.get 0x204 m |> Expect.equal ( Just 5 )
                ]
        )
    
    , test "op_FX65: Store registers V0 through Vx in memory starting at location I"
        (\_ ->
            let
                cpu =
                    { defaultCpu
                    | i = 0x200
                    , memory =
                        defaultMemory
                        |> Array.set 0x200 0x1
                        |> Array.set 0x201 0x2
                        |> Array.set 0x202 0x3
                        |> Array.set 0x203 0x4
                        |> Array.set 0x204 0x5
                    }
            in
            op_FX65 0xF465 cpu
            |> .registers
            |> Expect.all
                [ \r -> Array.get 0x0 r |> Expect.equal ( Just 1 )
                , \r -> Array.get 0x1 r |> Expect.equal ( Just 2 )
                , \r -> Array.get 0x2 r |> Expect.equal ( Just 3 )
                , \r -> Array.get 0x3 r |> Expect.equal ( Just 4 )
                , \r -> Array.get 0x4 r |> Expect.equal ( Just 5 )
                ]
        )
    ]