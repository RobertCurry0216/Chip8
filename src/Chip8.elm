module Chip8 exposing (..)

import Array exposing (Array)
import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy)
import Msg exposing (Msg(..))



--This might have to be replaced with an actual byte


type alias Byte =
    Int



--2 Bytes, might have to replaced with something else


type alias Word =
    Int


type alias RndStore =
    { vx : Byte
    , kk : Byte
    }


type alias Cpu =
    { memory : Array Byte
    , screenBuffer : Array Byte

    -- registers
    , registers : Array Byte
    , i : Word
    , pc : Word
    , stack : List Word

    -- timers
    , timerDelay : Byte
    , timerSound : Byte

    -- input
    , keys : Array Bool
    , wait : Bool
    , waitRegister : Byte

    -- random numbers
    , rnd : RndStore
    }


defaultMemory : Array Byte
defaultMemory =
    List.repeat 0x0FAF -1
        |> List.append hexSprites
        |> Array.fromList


emptyBuffer : Array Byte
emptyBuffer =
    List.repeat (64 * 32) 0 |> Array.fromList


emptyRegisters : Array Byte
emptyRegisters =
    List.repeat 0x10 0 |> Array.fromList


emptyKeys : Array Bool
emptyKeys =
    List.repeat 16 False |> Array.fromList


hexSprites : List Byte
hexSprites =
    [ 0xF0, 0x90, 0x90, 0x90, 0xF0 -- 0
    , 0x20, 0x60, 0x20, 0x20, 0x70 -- 1
    , 0xF0, 0x10, 0xF0, 0x80, 0xF0 -- 2
    , 0xF0, 0x10, 0xF0, 0x10, 0xF0 -- 3
    , 0x90, 0x90, 0xF0, 0x10, 0x10 -- 4
    , 0xF0, 0x80, 0xF0, 0x10, 0xF0 -- 5
    , 0xF0, 0x80, 0xF0, 0x90, 0xF0 -- 6
    , 0xF0, 0x10, 0x20, 0x40, 0x40 -- 7
    , 0xF0, 0x90, 0xF0, 0x90, 0xF0 -- 8
    , 0xF0, 0x90, 0xF0, 0x10, 0xF0 -- 9
    , 0xF0, 0x90, 0xF0, 0x90, 0x90 -- A
    , 0xE0, 0x90, 0xE0, 0x90, 0xE0 -- B
    , 0xF0, 0x80, 0x80, 0x80, 0xF0 -- C
    , 0xE0, 0x90, 0x90, 0x90, 0xE0 -- D
    , 0xF0, 0x80, 0xF0, 0x80, 0xF0 -- E
    , 0xF0, 0x80, 0xF0, 0x80, 0x80 -- F
    ]


defaultScreen : Array Byte
defaultScreen =
    Array.fromList 
    [ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    , 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,1,0,0,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1
    , 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    , 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
    ]


defaultCpu : Cpu
defaultCpu =
    { memory = defaultMemory
    , screenBuffer = emptyBuffer
    , registers = emptyRegisters
    , i = 0
    , pc = 0x0200
    , stack = []
    , timerDelay = 0
    , timerSound = 0
    , keys = emptyKeys
    , wait = False
    , waitRegister = 0
    , rnd = { vx = 0, kk = 0x0F }
    }



---- Bitwise helpers ----


toBitList : Byte -> List Int
toBitList int =
    [ shiftRightBy 7 int |> and 1
    , shiftRightBy 6 int |> and 1
    , shiftRightBy 5 int |> and 1
    , shiftRightBy 4 int |> and 1
    , shiftRightBy 3 int |> and 1
    , shiftRightBy 2 int |> and 1
    , shiftRightBy 1 int |> and 1
    , and 1 int
    ]


overflow8 : Int -> Byte
overflow8 =
    and 0x0100
        >> shiftRightBy 8


overflow16 : Int -> Word
overflow16 =
    and 0x00010000
        >> shiftRightBy 16


operater8 : (Byte -> Byte -> Byte) -> Byte -> Byte -> ( Byte, Int )
operater8 op a b =
    let
        v =
            op a b
    in
    ( and 0xFF v, overflow8 v )


operater16 : (Word -> Word -> Word) -> Word -> Word -> ( Word, Int )
operater16 op a b =
    let
        v =
            op a b
    in
    ( and 0xFFFF v, overflow16 v )


add8 : Byte -> Byte -> ( Byte, Int )
add8 =
    operater8 (+)


add16 : Word -> Word -> ( Word, Int )
add16 =
    operater16 (+)


sub8 : Byte -> Byte -> ( Byte, Int )
sub8 =
    operater8 (-)


shr8 : Byte -> ( Byte, Int )
shr8 value =
    ( shiftRightBy 1 value, and 1 value )


shl8 : Byte -> ( Byte, Int )
shl8 =
    operater8 shiftLeftBy 1


get0NNN : Word -> Int
get0NNN =
    and 0x0FFF


get0X00 : Word -> Int
get0X00 =
    and 0x0F00
        >> shiftRightBy 8


get00Y0 : Word -> Int
get00Y0 =
    and 0xF0
        >> shiftRightBy 4


get000N : Word -> Int
get000N =
    and 0x0F


get00KK : Word -> Int
get00KK =
    and 0xFF


getNextOpcode : Cpu -> Result String ( Word, Cpu )
getNextOpcode cpu =
    let
        byte1 =
            Array.get cpu.pc cpu.memory

        byte2 =
            Array.get (cpu.pc + 1) cpu.memory
    in
    case ( byte1, byte2 ) of
        ( Just b1, Just b2 ) ->
            Ok
                ( shiftLeftBy 8 b1 |> or b2
                , { cpu | pc = and (cpu.pc + 2) 0x0FFF }
                )

        _ ->
            Err "Error getting Op Code"


doOp : Word -> Cpu -> ( Cpu, Msg )
doOp opcode =
    case and 0xF000 opcode of
        0x00 ->
            case opcode of
                0xE0 ->
                    op_00E0 opcode >> (\cpu -> ( cpu, Continue ))

                0xEE ->
                    op_00EE opcode >> (\cpu -> ( cpu, Continue ))

                _ ->
                    op_0NNN opcode >> (\cpu -> ( cpu, Continue ))

        0x1000 ->
            op_1NNN opcode >> (\cpu -> ( cpu, Continue ))

        0x2000 ->
            op_2NNN opcode >> (\cpu -> ( cpu, Continue ))

        0x3000 ->
            op_3XKK opcode >> (\cpu -> ( cpu, Continue ))

        0x4000 ->
            op_4XKK opcode >> (\cpu -> ( cpu, Continue ))

        0x5000 ->
            op_5XY0 opcode >> (\cpu -> ( cpu, Continue ))

        0x6000 ->
            op_6XKK opcode >> (\cpu -> ( cpu, Continue ))

        0x7000 ->
            op_7XKK opcode >> (\cpu -> ( cpu, Continue ))

        0x8000 ->
            case and 0x0F opcode of
                0x00 ->
                    op_8XY0 opcode >> (\cpu -> ( cpu, Continue ))

                0x01 ->
                    op_8XY1 opcode >> (\cpu -> ( cpu, Continue ))

                0x02 ->
                    op_8XY2 opcode >> (\cpu -> ( cpu, Continue ))

                0x03 ->
                    op_8XY3 opcode >> (\cpu -> ( cpu, Continue ))

                0x04 ->
                    op_8XY4 opcode >> (\cpu -> ( cpu, Continue ))

                0x05 ->
                    op_8XY5 opcode >> (\cpu -> ( cpu, Continue ))

                0x06 ->
                    op_8XY6 opcode >> (\cpu -> ( cpu, Continue ))

                0x07 ->
                    op_8XY7 opcode >> (\cpu -> ( cpu, Continue ))

                0x0E ->
                    op_8XYE opcode >> (\cpu -> ( cpu, Continue ))

                _ ->
                    noop opcode >> (\cpu -> ( cpu, SetEmulatorRun False ))

        0x9000 ->
            op_9XY0 opcode >> (\cpu -> ( cpu, Continue ))

        0xA000 ->
            op_ANNN opcode >> (\cpu -> ( cpu, Continue ))

        0xB000 ->
            op_BNNN opcode >> (\cpu -> ( cpu, Continue ))

        0xC000 ->
            op_CXKK opcode >> (\cpu -> ( cpu, FetchRandom 0 255 ))

        0xD000 ->
            op_DXYN opcode >> (\cpu -> ( cpu, Continue ))

        0xE000 ->
            case and 0xFF opcode of
                0x9E ->
                    op_EX9E opcode >> (\cpu -> ( cpu, Continue ))

                0xA1 ->
                    op_EXA1 opcode >> (\cpu -> ( cpu, Continue ))

                _ ->
                    noop opcode >> (\cpu -> ( cpu, SetEmulatorRun False ))

        0xF000 ->
            case and 0xFF opcode of
                0x07 ->
                    op_FX07 opcode >> (\cpu -> ( cpu, Continue ))

                0x0A ->
                    op_FX0A opcode >> (\cpu -> ( cpu, Continue ))

                0x15 ->
                    op_FX15 opcode >> (\cpu -> ( cpu, Continue ))

                0x18 ->
                    op_FX18 opcode >> (\cpu -> ( cpu, Continue ))

                0x1E ->
                    op_FX1E opcode >> (\cpu -> ( cpu, Continue ))

                0x29 ->
                    op_FX29 opcode >> (\cpu -> ( cpu, Continue ))

                0x33 ->
                    op_FX33 opcode >> (\cpu -> ( cpu, Continue ))

                0x55 ->
                    op_FX55 opcode >> (\cpu -> ( cpu, Continue ))

                0x65 ->
                    op_FX65 opcode >> (\cpu -> ( cpu, Continue ))

                _ ->
                    noop opcode >> (\cpu -> ( cpu, SetEmulatorRun False ))

        _ ->
            noop opcode >> (\cpu -> ( cpu, SetEmulatorRun False ))



---- Cpu Helpers ----


doNextOp : Cpu -> ( Cpu, Msg )
doNextOp cpuIn =
    if cpuIn.wait then
        ( cpuIn, Continue )

    else
        case getNextOpcode cpuIn of
            Ok ( opcode, cpu ) ->
                doOp opcode cpu

            Err _ ->
                ( noop 0x00 cpuIn, SetEmulatorRun False )


updateTimers : Cpu -> Cpu
updateTimers cpuIn =
    { cpuIn
        | timerDelay = max 0 (cpuIn.timerDelay - 1)
        , timerSound = max 0 (cpuIn.timerSound - 1)
    }


loadIntoMemory : Cpu -> Word -> List Byte -> Cpu
loadIntoMemory cpuIn start list =
    list
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( i, v ) cpu ->
                Array.set (start + i) v cpu.memory
                    |> (\mem -> { cpu | memory = mem })
            )
            cpuIn


getRegValue : Cpu -> Byte -> Byte
getRegValue cpu vx =
    cpu.registers
        |> Array.get vx
        |> Maybe.withDefault 0



---- Op Code Functions ----


noop : Word -> Cpu -> Cpu
noop _ cpu =
    cpu



-- 0nnn - SYS addr
-- Jump to a machine code routine at nnn.


op_0NNN : Word -> Cpu -> Cpu
op_0NNN _ cpu =
    cpu



-- 00E0 - CLS
-- Clear the display.


op_00E0 : Word -> Cpu -> Cpu
op_00E0 _ cpu =
    { cpu | screenBuffer = emptyBuffer }



-- 00EE - RET
-- Return from a subroutine.
-- The interpreter sets the program counter to the address at the top of the stack, then subtracts 1 from the stack pointer.


op_00EE : Word -> Cpu -> Cpu
op_00EE _ cpu =
    case cpu.stack of
        pc :: stack ->
            { cpu
                | stack = stack
                , pc = pc
            }

        _ ->
            cpu



-- 1nnn - JP addr
-- Jump to location nnn.
-- The interpreter sets the program counter to nnn.


op_1NNN : Word -> Cpu -> Cpu
op_1NNN opcode cpu =
    { cpu | pc = get0NNN opcode }



-- 2nnn - CALL addr
-- Call subroutine at nnn.
-- The interpreter increments the stack pointer, then puts the current PC on the top of the stack. The PC is then set to nnn.


op_2NNN : Word -> Cpu -> Cpu
op_2NNN opcode cpu =
    { cpu
        | stack = cpu.pc :: cpu.stack
        , pc = get0NNN opcode
    }



-- 3xkk - SE Vx, byte
-- Skip next instruction if Vx = kk.
-- The interpreter compares register Vx to kk, and if they are equal, increments the program counter by 2.


op_3XKK : Word -> Cpu -> Cpu
op_3XKK opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        kk =
            get00KK opcode
    in
    if vx == kk then
        { cpu | pc = cpu.pc + 2 }

    else
        cpu



-- 4xkk - SNE Vx, byte
-- Skip next instruction if Vx != kk.
-- The interpreter compares register Vx to kk, and if they are not equal, increments the program counter by 2.


op_4XKK : Word -> Cpu -> Cpu
op_4XKK opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        kk =
            get00KK opcode
    in
    if vx /= kk then
        { cpu | pc = cpu.pc + 2 }

    else
        cpu



-- 5xy0 - SE Vx, Vy
-- Skip next instruction if Vx = Vy.
-- The interpreter compares register Vx to register Vy, and if they are equal, increments the program counter by 2.


op_5XY0 : Word -> Cpu -> Cpu
op_5XY0 opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        vy =
            getRegValue cpu (get00Y0 opcode)
    in
    if vx == vy then
        { cpu | pc = cpu.pc + 2 }

    else
        cpu



-- 6xkk - LD Vx, byte
-- Set Vx = kk.
-- The interpreter puts the value kk into register Vx.


op_6XKK : Word -> Cpu -> Cpu
op_6XKK opcode cpu =
    let
        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) (get00KK opcode)
    in
    { cpu | registers = registers }



-- 7xkk - ADD Vx, byte
-- Set Vx = Vx + kk.
-- Adds the value kk to the value of register Vx, then stores the result in Vx.


op_7XKK : Word -> Cpu -> Cpu
op_7XKK opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        kk =
            get00KK opcode

        ( value, _ ) =
            add8 vx kk

        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) value
    in
    { cpu | registers = registers }



-- 8xy0 - LD Vx, Vy
-- Set Vx = Vy.
-- Stores the value of register Vy in register Vx.


op_8XY0 : Word -> Cpu -> Cpu
op_8XY0 opcode cpu =
    let
        vy =
            getRegValue cpu (get00Y0 opcode)

        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) vy
    in
    { cpu | registers = registers }



-- 8xy1 - OR Vx, Vy
-- Set Vx = Vx OR Vy.
-- Performs a bitwise OR on the values of Vx and Vy, then stores the result in Vx. A bitwise OR compares the corrseponding bits from two values, and if either bit is 1, then the same bit in the result is also 1. Otherwise, it is 0.


op_8XY1 : Word -> Cpu -> Cpu
op_8XY1 opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        vy =
            getRegValue cpu (get00Y0 opcode)

        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) (or vx vy)
    in
    { cpu | registers = registers }



-- 8xy2 - AND Vx, Vy
-- Set Vx = Vx AND Vy.
-- Performs a bitwise AND on the values of Vx and Vy, then stores the result in Vx. A bitwise AND compares the corrseponding bits from two values, and if both bits are 1, then the same bit in the result is also 1. Otherwise, it is 0.


op_8XY2 : Word -> Cpu -> Cpu
op_8XY2 opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        vy =
            getRegValue cpu (get00Y0 opcode)

        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) (and vx vy)
    in
    { cpu | registers = registers }



-- 8xy3 - XOR Vx, Vy
-- Set Vx = Vx XOR Vy.
-- Performs a bitwise exclusive OR on the values of Vx and Vy, then stores the result in Vx. An exclusive OR compares the corrseponding bits from two values, and if the bits are not both the same, then the corresponding bit in the result is set to 1. Otherwise, it is 0.


op_8XY3 : Word -> Cpu -> Cpu
op_8XY3 opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        vy =
            getRegValue cpu (get00Y0 opcode)

        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) (Bitwise.xor vx vy)
    in
    { cpu | registers = registers }



-- 8xy4 - ADD Vx, Vy
-- Set Vx = Vx + Vy, set VF = carry.
-- The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. Only the lowest 8 bits of the result are kept, and stored in Vx.


op_8XY4 : Word -> Cpu -> Cpu
op_8XY4 opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        vy =
            getRegValue cpu (get00Y0 opcode)

        ( value, overflow ) =
            add8 vx vy

        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) value
                |> Array.set 0x0F overflow
    in
    { cpu | registers = registers }



-- 8xy5 - SUB Vx, Vy
-- Set Vx = Vx - Vy, set VF = NOT borrow.
-- If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from Vx, and the results stored in Vx.


op_8XY5 : Word -> Cpu -> Cpu
op_8XY5 opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        vy =
            getRegValue cpu (get00Y0 opcode)

        ( value, _ ) =
            sub8 vx vy

        overflow =
            if vx > vy then
                1

            else
                0

        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) value
                |> Array.set 0x0F overflow
    in
    { cpu | registers = registers }



-- 8xy6 - SHR Vx {, Vy}
-- Set Vx = Vx SHR 1.
-- If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. Then Vx is divided by 2.


op_8XY6 : Word -> Cpu -> Cpu
op_8XY6 opcode cpu =
    let
        vy =
            getRegValue cpu (get00Y0 opcode)

        ( value, overflow ) =
            shr8 vy

        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) value
                |> Array.set 0x0F overflow
    in
    { cpu | registers = registers }



-- 8xy7 - SUBN Vx, Vy
-- Set Vx = Vy - Vx, set VF = NOT borrow.
-- If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from Vy, and the results stored in Vx.


op_8XY7 : Word -> Cpu -> Cpu
op_8XY7 opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        vy =
            getRegValue cpu (get00Y0 opcode)

        ( value, _ ) =
            sub8 vy vx

        overflow =
            if vy > vx then
                1

            else
                0

        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) value
                |> Array.set 0x0F overflow
    in
    { cpu | registers = registers }



-- 8xyE - SHL Vx {, Vy}
-- Set Vx = Vx SHL 1.
-- If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0. Then Vx is multiplied by 2.


op_8XYE : Word -> Cpu -> Cpu
op_8XYE opcode cpu =
    let
        vy =
            getRegValue cpu (get00Y0 opcode)

        ( value, overflow ) =
            shl8 vy

        registers =
            cpu.registers
                |> Array.set (get0X00 opcode) value
                |> Array.set 0x0F overflow
    in
    { cpu | registers = registers }



-- 9xy0 - SNE Vx, Vy
-- Skip next instruction if Vx != Vy.
-- The values of Vx and Vy are compared, and if they are not equal, the program counter is increased by 2.


op_9XY0 : Word -> Cpu -> Cpu
op_9XY0 opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        vy =
            getRegValue cpu (get00Y0 opcode)
    in
    if vx /= vy then
        { cpu | pc = cpu.pc + 2 }

    else
        cpu



-- Annn - LD I, addr
-- Set I = nnn.
-- The value of register I is set to nnn.


op_ANNN : Word -> Cpu -> Cpu
op_ANNN opcode cpu =
    { cpu | i = get0NNN opcode }



-- Bnnn - JP V0, addr
-- Jump to location nnn + V0.
-- The program counter is set to nnn plus the value of V0.


op_BNNN : Word -> Cpu -> Cpu
op_BNNN opcode cpu =
    let
        v0 =
            getRegValue cpu 0

        nnn =
            get0NNN opcode

        ( pc, _ ) =
            add16 v0 nnn
    in
    { cpu | pc = pc }



-- Cxkk - RND Vx, byte
-- Set Vx = random byte AND kk.
-- The interpreter generates a random number from 0 to 255, which is then ANDed with the value kk. The results are stored in Vx. See instruction 8xy2 for more information on AND.


insertRnd : Byte -> Cpu -> Cpu
insertRnd rnd cpu =
    let
        registers =
            cpu.registers
                |> Array.set cpu.rnd.vx (and cpu.rnd.kk rnd)
    in
    { cpu
        | registers = registers
    }


op_CXKK : Word -> Cpu -> Cpu
op_CXKK opcode cpuIn =
    { cpuIn
        | rnd =
            { vx = get0X00 opcode
            , kk = get00KK opcode
            }
    }



-- Dxyn - DRW Vx, Vy, nibble
-- Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
-- The interpreter reads n bytes from memory, starting at the address stored in I. These bytes are then displayed as sprites on screen at coordinates (Vx, Vy). Sprites are XORed onto the existing screen. If this causes any pixels to be erased, VF is set to 1, otherwise it is set to 0. If the sprite is positioned so part of it is outside the coordinates of the display, it wraps around to the opposite side of the screen. See instruction 8xy3 for more information on XOR, and section 2.4, Display, for more information on the Chip-8 screen and sprites.


printBitToScreen : Int -> Array Byte -> Byte -> Byte -> ( Bool, Array Byte )
printBitToScreen idx buffer bit oldBit =
    let
        newBit =
            Bitwise.xor bit oldBit

        collision =
            Bitwise.and bit oldBit == 1

        arrayOut =
            Array.set idx newBit buffer
    in
    ( collision, arrayOut )


printByteToScreen : Cpu -> Int -> Int -> Int -> ( Cpu, Bool )
printByteToScreen cpu x y byte =
    let
        idx =
            x + y * 64

        bits =
            byte
                |> toBitList
                |> List.indexedMap (\a b -> ( a, b ))

        ( collision, screenBuffer ) =
            bits
                |> List.foldl
                    (\( i, b ) ( col, buffer ) ->
                        Array.get (i + idx) buffer
                            |> Maybe.withDefault 0
                            |> printBitToScreen (i + idx) buffer b
                            |> (\( c, buf ) -> ( col || c, buf ))
                    )
                    ( False, cpu.screenBuffer )
    in
    ( { cpu | screenBuffer = screenBuffer }, collision )


op_DXYN : Word -> Cpu -> Cpu
op_DXYN opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        vy =
            getRegValue cpu (get00Y0 opcode)

        ( newcpu, collision ) =
            get000N opcode
                |> (+) -1
                |> List.range 0
                |> List.foldl
                    (\off ( cpuIn, col ) ->
                        Array.get (cpuIn.i + off) cpuIn.memory
                            |> Maybe.withDefault 0
                            |> printByteToScreen cpuIn vx (vy + off)
                            |> (\( cpuOut, c ) -> ( cpuOut, c || col ))
                    )
                    ( cpu, False )

        colByte =
            if collision then
                1

            else
                0
    in
    { newcpu
        | registers = Array.set 0x0F colByte newcpu.registers
    }



-- Ex9E - SKP Vx
-- Skip next instruction if key with the value of Vx is pressed.
-- Checks the keyboard, and if the key corresponding to the value of Vx is currently in the down position, PC is increased by 2.


op_EX9E : Word -> Cpu -> Cpu
op_EX9E opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        keydown =
            cpu.keys
                |> Array.get vx
                |> Maybe.withDefault False
    in
    if keydown then
        { cpu | pc = and (cpu.pc + 2) 0x0FFF }

    else
        cpu



-- ExA1 - SKNP Vx
-- Skip next instruction if key with the value of Vx is not pressed.
-- Checks the keyboard, and if the key corresponding to the value of Vx is currently in the up position, PC is increased by 2.


op_EXA1 : Word -> Cpu -> Cpu
op_EXA1 opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        keydown =
            cpu.keys
                |> Array.get vx
                |> Maybe.withDefault False
    in
    if keydown then
        cpu

    else
        { cpu | pc = and (cpu.pc + 2) 0x0FFF }



-- Fx07 - LD Vx, DT
-- Set Vx = delay timer value.
-- The value of DT is placed into Vx.


op_FX07 : Word -> Cpu -> Cpu
op_FX07 opcode cpu =
    let
        registers =
            Array.set (get0X00 opcode) cpu.timerDelay cpu.registers
    in
    { cpu
        | registers = registers
    }



-- Fx0A - LD Vx, K
-- Wait for a key press, store the value of the key in Vx.
-- All execution SetEmulatorRun Falses until a key is pressed, then the value of that key is stored in Vx.


op_FX0A : Word -> Cpu -> Cpu
op_FX0A opcode cpu =
    { cpu
        | wait = True
        , waitRegister = get0X00 opcode
    }


endWait : Byte -> Cpu -> Cpu
endWait key cpu =
    if cpu.wait then
        { cpu
            | wait = False
            , registers = Array.set cpu.waitRegister key cpu.registers
        }

    else
        cpu



-- Fx15 - LD DT, Vx
-- Set delay timer = Vx.
-- DT is set equal to the value of Vx.


op_FX15 : Word -> Cpu -> Cpu
op_FX15 opcode cpu =
    { cpu
        | timerDelay = getRegValue cpu (get0X00 opcode)
    }



-- Fx18 - LD ST, Vx
-- Set sound timer = Vx.
-- ST is set equal to the value of Vx.


op_FX18 : Word -> Cpu -> Cpu
op_FX18 opcode cpu =
    { cpu
        | timerSound = getRegValue cpu (get0X00 opcode)
    }



-- Fx1E - ADD I, Vx
-- Set I = I + Vx.
-- The values of I and Vx are added, and the results are stored in I.


op_FX1E : Word -> Cpu -> Cpu
op_FX1E opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)

        ( value, _ ) =
            add16 vx cpu.i
    in
    { cpu | i = value }



-- Fx29 - LD F, Vx
-- Set I = location of sprite for digit Vx.
-- The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx. See section 2.4, Display, for more information on the Chip-8 hexadecimal font.


op_FX29 : Word -> Cpu -> Cpu
op_FX29 opcode cpu =
    let
        vx =
            getRegValue cpu (get0X00 opcode)
    in
    { cpu
        | i = 5 * vx
    }



-- Fx33 - LD B, Vx
-- Store BCD representation of Vx in memory locations I, I+1, and I+2.
-- The interpreter takes the decimal value of Vx, and places the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2.


op_FX33 : Word -> Cpu -> Cpu
op_FX33 opcode cpu =
    let
        value =
            getRegValue cpu (get0X00 opcode)

        hundreds =
            value // 100

        tens =
            value
                // 10
                |> Basics.modBy 10

        ones =
            Basics.modBy 10 value

        memory =
            cpu.memory
                |> Array.set cpu.i hundreds
                |> Array.set (cpu.i + 1) tens
                |> Array.set (cpu.i + 2) ones
    in
    { cpu
        | memory = memory
    }



-- Fx55 - LD [I], Vx
-- Store registers V0 through Vx in memory starting at location I.
-- The interpreter copies the values of registers V0 through Vx into memory, starting at the address in I.


op_FX55 : Word -> Cpu -> Cpu
op_FX55 opcode cpu =
    cpu.registers
        |> Array.toList
        |> List.take (get0X00 opcode + 1)
        |> loadIntoMemory cpu cpu.i
        |> (\cpuOut -> { cpuOut | i = cpuOut.i + get0X00 opcode + 1 })



-- Fx65 - LD Vx, [I]
-- Read registers V0 through Vx from memory starting at location I.
-- The interpreter reads values from memory starting at location I into registers V0 through Vx.


op_FX65 : Word -> Cpu -> Cpu
op_FX65 opcode cpu =
    let
        x =
            get0X00 opcode
    in
    x
        + 1
        |> List.range 0
        |> List.foldl
            (\off regIn ->
                Array.get (cpu.i + off) cpu.memory
                    |> Maybe.withDefault 0
                    |> (\v -> Array.set off v regIn)
            )
            cpu.registers
        |> (\reg -> { cpu | registers = reg, i = cpu.i + x + 1 })
