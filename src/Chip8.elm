module Chip8 exposing 
  ( Cpu
  , Byte
  , ChipMsg(..)
  , defaultCpu
  , doNextOp
  , loadIntoMemory
  , emptyBuffer
  , updateTimers
  )
import Array exposing (Array)
import Bitwise exposing (shiftLeftBy, shiftRightBy, or, and)


type ChipMsg
  = Continue
  | InsertRandomInt


--This might have to be replaced with an actual byte
type alias Byte = Int


--2 Bytes, might have to replaced with something else
type alias Word = Int


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
  }


defaultMemory : Array Byte
defaultMemory =
  List.repeat 0xFAF -1
  |> List.append hexSprites
  |> Array.fromList


emptyBuffer : Array Byte
emptyBuffer =
  List.repeat (64*32) 0 |> Array.fromList


emptyRegisters : Array Byte
emptyRegisters =
  List.repeat 0x010 0 |> Array.fromList


hexSprites : List Byte
hexSprites =
  [ 0xF0, 0x90, 0x90, 0x90, 0xF0
  , 0x20, 0x60, 0x20, 0x20, 0x70 
  , 0xF0, 0x10, 0xF0, 0x80, 0xF0 
  , 0xF0, 0x10, 0xF0, 0x10, 0xF0 
  , 0x90, 0x90, 0xF0, 0x10, 0x10 
  , 0xF0, 0x80, 0xF0, 0x10, 0xF0 
  , 0xF0, 0x80, 0xF0, 0x90, 0xF0 
  , 0xF0, 0x10, 0x20, 0x40, 0x40 
  , 0xF0, 0x90, 0xF0, 0x90, 0xF0 
  , 0xF0, 0x90, 0xF0, 0x10, 0xF0 
  , 0xF0, 0x90, 0xF0, 0x90, 0x90 
  , 0xE0, 0x90, 0xE0, 0x90, 0xE0 
  , 0xF0, 0x80, 0x80, 0x80, 0xF0 
  , 0xE0, 0x90, 0x90, 0x90, 0xE0 
  , 0xF0, 0x80, 0xF0, 0x80, 0xF0 
  , 0xF0, 0x80, 0xF0, 0x80, 0x80 ]


defaultCpu : Cpu
defaultCpu =
  { memory = defaultMemory
  , screenBuffer = emptyBuffer
  , registers = emptyRegisters
  , i = 0
  , pc = 0x200
  , stack = []
  , timerDelay = 0
  , timerSound = 0
  , keys = List.repeat 16 False |> Array.fromList
  , wait = False
  , waitRegister = 0
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
  and 0x100
  >> shiftRightBy 8


overflow16 : Int -> Word
overflow16 =
  and 0x10000
  >> shiftRightBy 16


operater8 : (Byte -> Byte -> Byte ) -> Byte -> Byte -> (Byte, Int)
operater8 op a b =
  let
      v = op a b
  in
  (and 0xFF v, overflow8 v)
  

operater16 : (Word -> Word -> Word ) -> Word -> Word -> (Word, Int)
operater16 op a b =
  let
      v = op a b
  in
  (and 0xFFFF v, overflow16 v)


add8 : Byte -> Byte -> (Byte, Int)
add8 = 
  operater8 (+)


add16 : Word -> Word -> (Word, Int)
add16 = 
  operater16 (+)


sub8 : Byte -> Byte -> (Byte, Int)
sub8 = 
  operater8 (-)


shr8 : Byte -> (Byte, Int)
shr8 value =
  (shiftRightBy 1 value, and 1 value)


shl8 : Byte -> (Byte, Int)
shl8 =
  operater8 ( shiftLeftBy ) 1


get0NNN : Word -> Int
get0NNN = 
  and 0x0FFF


get0N00 : Word -> Int
get0N00 =
  and 0x0F00
  >> shiftRightBy 8


get00N0 : Word -> Int
get00N0 =
  and 0x00F0
  >> shiftRightBy 4


get000N : Word -> Int
get000N =
  and 0x000F


get00NN : Word -> Int
get00NN =
  and 0x00FF


getNextOpcode : Cpu -> Result String (Word, Cpu)
getNextOpcode cpu =
  let
    byte1 =
      Array.get cpu.pc cpu.memory

    byte2 =
      Array.get (cpu.pc + 1) cpu.memory

  in
  case (byte1, byte2) of
    (Just b1, Just b2) ->
      Ok (  shiftLeftBy 8 b1 |> or b2
          , { cpu | pc = cpu.pc + 2 }
          )
    _ ->
      Err "Error getting Op Code"


doOp : Word -> Cpu -> (ChipMsg, Cpu)
doOp opcode =
  case and 0xF000 opcode of
    0x0000 ->
      case opcode of
      0x00E0 -> op_00E0 opcode >> Tuple.pair Continue
      0x00EE -> op_00EE opcode >> Tuple.pair Continue
      _ -> op_0NNN opcode >> Tuple.pair Continue
    0x1000 -> op_1NNN opcode >> Tuple.pair Continue
    0x2000 -> op_2NNN opcode >> Tuple.pair Continue
    0x3000 -> op_3XKK opcode >> Tuple.pair Continue
    0x4000 -> op_4XKK opcode >> Tuple.pair Continue
    0x5000 -> op_5XY0 opcode >> Tuple.pair Continue
    0x6000 -> op_6XKK opcode >> Tuple.pair Continue
    0x7000 -> op_7XKK opcode >> Tuple.pair Continue
    0x8000 ->
      case and 0x000F opcode of
      0x0000 -> op_8XY0 opcode >> Tuple.pair Continue
      0x0001 -> op_8XY1 opcode >> Tuple.pair Continue
      0x0002 -> op_8XY2 opcode >> Tuple.pair Continue
      0x0003 -> op_8XY3 opcode >> Tuple.pair Continue
      0x0004 -> op_8XY4 opcode >> Tuple.pair Continue
      0x0005 -> op_8XY5 opcode >> Tuple.pair Continue
      0x0006 -> op_8XY6 opcode >> Tuple.pair Continue
      0x0007 -> op_8XY7 opcode >> Tuple.pair Continue
      0x000E -> op_8XYE opcode >> Tuple.pair Continue
      _ -> noop opcode >> Tuple.pair Continue
    0x9000 -> op_9XY0 opcode >> Tuple.pair Continue
    0xA000 -> op_ANNN opcode >> Tuple.pair Continue
    0xB000 -> op_BNNN opcode >> Tuple.pair Continue
    0xC000 -> op_CXKK opcode >> Tuple.pair InsertRandomInt
    0xD000 -> op_DXYN opcode >> Tuple.pair Continue
    0xE000 ->
      case and 0x00FF opcode of
      0x009E -> op_EX9E opcode >> Tuple.pair Continue
      0x00A1 -> op_EXA1 opcode >> Tuple.pair Continue
      _ -> noop opcode >> Tuple.pair Continue
    0xF000 ->
      case and 0x00FF opcode of
      0x0007 -> op_FX07 opcode >> Tuple.pair Continue
      0x000A -> op_FX0A opcode >> Tuple.pair Continue
      0x0015 -> op_FX15 opcode >> Tuple.pair Continue
      0x0018 -> op_FX18 opcode >> Tuple.pair Continue
      0x001E -> op_FX1E opcode >> Tuple.pair Continue
      0x0029 -> op_FX29 opcode >> Tuple.pair Continue
      0x0033 -> op_FX33 opcode >> Tuple.pair Continue
      0x0055 -> op_FX55 opcode >> Tuple.pair Continue
      0x0065 -> op_FX65 opcode >> Tuple.pair Continue
      _ -> noop opcode >> Tuple.pair Continue
    _ -> noop opcode >> Tuple.pair Continue

---- Cpu Helpers ----

doNextOp : Cpu -> (ChipMsg, Cpu)
doNextOp cpuIn =
  if cpuIn.wait then
    (Continue, cpuIn)
  else
    case getNextOpcode cpuIn of
    Ok (opcode, cpu) ->
      doOp opcode cpu
    Err _ ->
      (Continue, noop 0x0000 cpuIn)


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
    (\(i, v) cpu ->
      Array.set (start + i) v cpu.memory
      |> (\mem -> {cpu | memory = mem})
    ) cpuIn


getRegValue : Cpu -> Byte -> Byte
getRegValue cpu vx =
  cpu.registers
    |> Array.get vx
    |> Maybe.withDefault 0

---- Op Code Functions ----

noop : Word -> Cpu -> Cpu
noop opcode cpu =
  Debug.log ("noop " ++ String.fromInt opcode)
  cpu

-- 0nnn - SYS addr
-- Jump to a machine code routine at nnn.
op_0NNN : Word -> Cpu -> Cpu
op_0NNN opcode cpu =
  Debug.log "op_0NNN"
  {cpu | pc = get0NNN opcode}


-- 00E0 - CLS
-- Clear the display.
op_00E0 : Word -> Cpu -> Cpu
op_00E0 _ cpu =
  Debug.log "op_00E0"
  { cpu | screenBuffer = emptyBuffer }


-- 00EE - RET
-- Return from a subroutine.
-- The interpreter sets the program counter to the address at the top of the stack, then subtracts 1 from the stack pointer.
op_00EE : Word -> Cpu -> Cpu
op_00EE _ cpu =
  case cpu.stack of
  pc::stack ->
    Debug.log "op_00EE"
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
  Debug.log ("op_1NNN " ++ (String.fromInt opcode))
  {cpu | pc = get0NNN opcode}


-- 2nnn - CALL addr
-- Call subroutine at nnn.
-- The interpreter increments the stack pointer, then puts the current PC on the top of the stack. The PC is then set to nnn.
op_2NNN : Word -> Cpu -> Cpu
op_2NNN opcode cpu =
  Debug.log "op_2NNN"
  { cpu
  | stack = cpu.pc::cpu.stack
  , pc = get0NNN opcode
  }


-- 3xkk - SE Vx, byte
-- Skip next instruction if Vx = kk.
-- The interpreter compares register Vx to kk, and if they are equal, increments the program counter by 2.
op_3XKK : Word -> Cpu -> Cpu
op_3XKK opcode cpu =
  let
    vx =
      cpu.registers
      |> Array.get ( get0N00 opcode )
      |> Maybe.withDefault -1

    kk =
      get00NN opcode
  in
  if vx == kk then
    Debug.log "op_3XKK"
    { cpu | pc = cpu.pc + 2 }
  else
    Debug.log "op_3XKK"
    cpu


-- 4xkk - SNE Vx, byte
-- Skip next instruction if Vx != kk.
-- The interpreter compares register Vx to kk, and if they are not equal, increments the program counter by 2.
op_4XKK : Word -> Cpu -> Cpu
op_4XKK opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    kk =
      get00NN opcode
  in
  if vx /= kk then
    Debug.log "op_4XKK"
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
      getRegValue cpu ( get0N00 opcode )

    vy =
      getRegValue cpu ( get00N0 opcode )
  in
  if vx == vy then
    Debug.log "op_5XY0"
    { cpu | pc = cpu.pc + 2 }
  else
    Debug.log "op_5XY0"
    cpu


-- 6xkk - LD Vx, byte
-- Set Vx = kk.
-- The interpreter puts the value kk into register Vx.
op_6XKK : Word -> Cpu -> Cpu
op_6XKK opcode cpu =
  let
    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) ( get00NN opcode ) 
  in
  Debug.log "op_6XKK"
  { cpu | registers = registers }


-- 7xkk - ADD Vx, byte
-- Set Vx = Vx + kk.
-- Adds the value kk to the value of register Vx, then stores the result in Vx.
op_7XKK : Word -> Cpu -> Cpu
op_7XKK opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    kk =
      get00NN opcode

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) ( vx + kk )
  in
  Debug.log "op_7XKK"
  { cpu | registers = registers }

-- 8xy0 - LD Vx, Vy
-- Set Vx = Vy.
-- Stores the value of register Vy in register Vx.
op_8XY0 : Word -> Cpu -> Cpu
op_8XY0 opcode cpu =
  let
    vy =
      getRegValue cpu ( get00N0 opcode )

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) vy
  in
  Debug.log "op_8XY0"
  { cpu | registers = registers }


-- 8xy1 - OR Vx, Vy
-- Set Vx = Vx OR Vy.
-- Performs a bitwise OR on the values of Vx and Vy, then stores the result in Vx. A bitwise OR compares the corrseponding bits from two values, and if either bit is 1, then the same bit in the result is also 1. Otherwise, it is 0.
op_8XY1 : Word -> Cpu -> Cpu
op_8XY1 opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    vy =
      getRegValue cpu ( get00N0 opcode )

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) ( or vx vy )

  in
  Debug.log "op_8XY1"
  { cpu | registers = registers }



-- 8xy2 - AND Vx, Vy
-- Set Vx = Vx AND Vy.
-- Performs a bitwise AND on the values of Vx and Vy, then stores the result in Vx. A bitwise AND compares the corrseponding bits from two values, and if both bits are 1, then the same bit in the result is also 1. Otherwise, it is 0.
op_8XY2 : Word -> Cpu -> Cpu
op_8XY2 opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    vy =
      getRegValue cpu ( get00N0 opcode )

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) ( and vx vy )

  in
  Debug.log "op_8XY2"
  { cpu | registers = registers }


-- 8xy3 - XOR Vx, Vy
-- Set Vx = Vx XOR Vy.
-- Performs a bitwise exclusive OR on the values of Vx and Vy, then stores the result in Vx. An exclusive OR compares the corrseponding bits from two values, and if the bits are not both the same, then the corresponding bit in the result is set to 1. Otherwise, it is 0.
op_8XY3 : Word -> Cpu -> Cpu
op_8XY3 opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    vy =
      getRegValue cpu ( get00N0 opcode )

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) ( Bitwise.xor vx vy )

  in
  Debug.log "op_8XY3"
  { cpu | registers = registers }


-- 8xy4 - ADD Vx, Vy
-- Set Vx = Vx + Vy, set VF = carry.
-- The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. Only the lowest 8 bits of the result are kept, and stored in Vx.
op_8XY4 : Word -> Cpu -> Cpu
op_8XY4 opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    vy =
      getRegValue cpu ( get00N0 opcode )

    (value, overflow) =
      add8 vx vy

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) value
      |> Array.set 0xF overflow

  in
  Debug.log "op_8XY4"
  { cpu | registers = registers }


-- 8xy5 - SUB Vx, Vy
-- Set Vx = Vx - Vy, set VF = NOT borrow.
-- If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from Vx, and the results stored in Vx.
op_8XY5 : Word -> Cpu -> Cpu
op_8XY5 opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    vy =
      getRegValue cpu ( get00N0 opcode )

    (value, overflow) =
      sub8 vx vy

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) value
      |> Array.set 0xF ( Bitwise.complement overflow)

  in
  Debug.log "op_8XY5"
  { cpu | registers = registers }


-- 8xy6 - SHR Vx {, Vy}
-- Set Vx = Vx SHR 1.
-- If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. Then Vx is divided by 2.
op_8XY6 : Word -> Cpu -> Cpu
op_8XY6 opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    (value, overflow) =
      shr8 vx

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) value
      |> Array.set 0xF overflow

  in
  Debug.log "op_8XY6"
  { cpu | registers = registers }


-- 8xy7 - SUBN Vx, Vy
-- Set Vx = Vy - Vx, set VF = NOT borrow.
-- If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from Vy, and the results stored in Vx.
op_8XY7 : Word -> Cpu -> Cpu
op_8XY7 opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    vy =
      getRegValue cpu ( get00N0 opcode )

    (value, overflow) =
      sub8 vy vx

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) value
      |> Array.set 0xF ( Bitwise.complement overflow)

  in
  Debug.log "op_8XY7"
  { cpu | registers = registers }


-- 8xyE - SHL Vx {, Vy}
-- Set Vx = Vx SHL 1.
-- If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0. Then Vx is multiplied by 2.
op_8XYE : Word -> Cpu -> Cpu
op_8XYE opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    (value, overflow) =
      shl8 vx

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) value
      |> Array.set 0xF overflow

  in
  Debug.log "op_8XYE"
  { cpu | registers = registers }


-- 9xy0 - SNE Vx, Vy
-- Skip next instruction if Vx != Vy.
-- The values of Vx and Vy are compared, and if they are not equal, the program counter is increased by 2.
op_9XY0 : Word -> Cpu -> Cpu
op_9XY0 opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    vy =
      getRegValue cpu ( get00N0 opcode )
  in
  if vx /= vy then
    Debug.log "op_9XY0"
    { cpu | pc = cpu.pc + 2 }
  else
    Debug.log "op_9XY0"
    cpu


-- Annn - LD I, addr
-- Set I = nnn.
-- The value of register I is set to nnn.
op_ANNN : Word -> Cpu -> Cpu
op_ANNN opcode cpu =
  Debug.log "op_ANNN"
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

    (pc, _) =
      add16 v0 nnn
  in
  Debug.log "op_BNNN"
  { cpu | pc = pc }


-- Cxkk - RND Vx, byte
-- Set Vx = random byte AND kk.
-- The interpreter generates a random number from 0 to 255, which is then ANDed with the value kk. The results are stored in Vx. See instruction 8xy2 for more information on AND.
op_CXKK : Word -> Cpu -> Cpu
op_CXKK opcode cpu =
  Debug.log "op_CXKK"
  Debug.todo "random numbers"


-- Dxyn - DRW Vx, Vy, nibble
-- Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
-- The interpreter reads n bytes from memory, starting at the address stored in I. These bytes are then displayed as sprites on screen at coordinates (Vx, Vy). Sprites are XORed onto the existing screen. If this causes any pixels to be erased, VF is set to 1, otherwise it is set to 0. If the sprite is positioned so part of it is outside the coordinates of the display, it wraps around to the opposite side of the screen. See instruction 8xy3 for more information on XOR, and section 2.4, Display, for more information on the Chip-8 screen and sprites.
printBitToScreen : Int -> Array Byte -> Byte  -> Byte -> (Bool, Array Byte)
printBitToScreen idx buffer bit oldBit =
  let
    newBit =
      Bitwise.xor bit oldBit

    collision =
      oldBit == 1 && newBit == 0
  in
  (collision, Array.set idx newBit buffer)
  

printByteToScreen : Cpu -> Int -> Int -> Int -> ( Cpu, Bool )
printByteToScreen cpu x y byte =
  let
    idx =
      x + y*64

    bits =
      byte
      |> toBitList
      |> List.indexedMap (\a b -> (a,b))

    (collision, screenBuffer) =
      bits
      |> List.foldl
        (\(i, b) (col, buffer) -> 
          Array.get (i + idx) buffer
          |> Maybe.withDefault 0
          |> printBitToScreen (i + idx) buffer b
          |> (\(c, buf) -> (col || c, buf))
        ) (False, cpu.screenBuffer)
  in
  ({ cpu | screenBuffer = screenBuffer }, collision)


op_DXYN : Word -> Cpu -> Cpu
op_DXYN opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    vy =
      getRegValue cpu ( get00N0 opcode )

    (newcpu, collision) =
      get000N opcode
      |> (+) -1
      |> List.range 0
      |> List.foldl (\off (c, _) ->
        Array.get (c.i + off) c.memory
        |> Maybe.withDefault 0
        |> printByteToScreen c vx (vy + off) 
      ) (cpu, False)

    colByte =
      if collision then 1 else 0
  in
  Debug.log "op_DXYN"
  { newcpu 
  | registers = Array.set 0xF colByte newcpu.registers 
  }


-- Ex9E - SKP Vx
-- Skip next instruction if key with the value of Vx is pressed.
-- Checks the keyboard, and if the key corresponding to the value of Vx is currently in the down position, PC is increased by 2.
op_EX9E : Word -> Cpu -> Cpu
op_EX9E opcode cpu =
  let
    keydown =
      cpu.keys
      |> Array.get ( get0N00 opcode)
      |> Maybe.withDefault False
  in
  if keydown then
    Debug.log "op_EX9E"
    { cpu | pc = cpu.pc + 2 }
  else 
    Debug.log "op_EX9E"
    cpu


-- ExA1 - SKNP Vx
-- Skip next instruction if key with the value of Vx is not pressed.
-- Checks the keyboard, and if the key corresponding to the value of Vx is currently in the up position, PC is increased by 2.
op_EXA1 : Word -> Cpu -> Cpu
op_EXA1 opcode cpu =
  let
    keydown =
      cpu.keys
      |> Array.get ( get0N00 opcode)
      |> Maybe.withDefault False
  in
  if keydown then
    Debug.log "op_EXA1"
    cpu
  else 
    Debug.log "op_EXA1"
    { cpu | pc = cpu.pc + 2 }


-- Fx07 - LD Vx, DT
-- Set Vx = delay timer value.
-- The value of DT is placed into Vx.
op_FX07 : Word -> Cpu -> Cpu
op_FX07 opcode cpu =
  let
    r =
      getRegValue cpu ( get0N00 opcode )  
  in
  Debug.log "op_FX07"
  { cpu 
  | registers = Array.set r cpu.timerDelay cpu.registers
  }


-- Fx0A - LD Vx, K
-- Wait for a key press, store the value of the key in Vx.
-- All execution stops until a key is pressed, then the value of that key is stored in Vx.
op_FX0A : Word -> Cpu -> Cpu
op_FX0A opcode cpu =
  Debug.log "op_FX0A"
  { cpu
  | wait = True
  , waitRegister = getRegValue cpu ( get0N00 opcode ) 
  }


-- Fx15 - LD DT, Vx
-- Set delay timer = Vx.
-- DT is set equal to the value of Vx.
op_FX15 : Word -> Cpu -> Cpu
op_FX15 opcode cpu =
  Debug.log "op_FX15"
  { cpu 
  | timerDelay = getRegValue cpu ( get0N00 opcode )
  }


-- Fx18 - LD ST, Vx
-- Set sound timer = Vx.
-- ST is set equal to the value of Vx.
op_FX18 : Word -> Cpu -> Cpu
op_FX18 opcode cpu =
  Debug.log "op_FX18"
  { cpu 
  | timerSound = getRegValue cpu ( get0N00 opcode )
  }


-- Fx1E - ADD I, Vx
-- Set I = I + Vx.
-- The values of I and Vx are added, and the results are stored in I.
op_FX1E : Word -> Cpu -> Cpu
op_FX1E opcode cpu =
  let
    vx =
      getRegValue cpu ( get0N00 opcode )

    (value, _) =
      add8 vx cpu.i
  in
  Debug.log "op_FX1E"
  { cpu | i = value }


-- Fx29 - LD F, Vx
-- Set I = location of sprite for digit Vx.
-- The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx. See section 2.4, Display, for more information on the Chip-8 hexadecimal font.
op_FX29 : Word -> Cpu -> Cpu
op_FX29 opcode cpu =
  Debug.log "op_FX29"
  {cpu
  | i = 5 * get0N00 opcode
  }


-- Fx33 - LD B, Vx
-- Store BCD representation of Vx in memory locations I, I+1, and I+2.
-- The interpreter takes the decimal value of Vx, and places the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2.
op_FX33 : Word -> Cpu -> Cpu
op_FX33 opcode cpu =
  let
    value =
      getRegValue cpu (get0N00 opcode)

    hundreds =
      value // 100

    tens =
      value // 10
      |> Basics.modBy 10

    ones =
      Basics.modBy 10 value

    registers =
      cpu.registers
      |> Array.set cpu.i hundreds
      |> Array.set (cpu.i + 1) tens
      |> Array.set (cpu.i + 2) ones

  in
  Debug.log "op_FX33"
  {cpu
  | registers = registers
  }


-- Fx55 - LD [I], Vx
-- Store registers V0 through Vx in memory starting at location I.
-- The interpreter copies the values of registers V0 through Vx into memory, starting at the address in I.
op_FX55 : Word -> Cpu -> Cpu
op_FX55 opcode cpu =
  Debug.log "op_FX55"
  cpu.registers
  |> Array.toList
  |> List.take (get0N00 opcode)
  |> loadIntoMemory cpu cpu.i


-- Fx65 - LD Vx, [I]
-- Read registers V0 through Vx from memory starting at location I.
-- The interpreter reads values from memory starting at location I into registers V0 through Vx.
op_FX65 : Word -> Cpu -> Cpu
op_FX65 opcode cpu =
  Debug.log "op_FX65"
  get0N00 opcode
  |> List.range 0
  |> List.foldl (\off regIn -> 
    Array.get (cpu.i + off) cpu.memory
    |> Maybe.withDefault 0
    |> \v -> Array.set off v regIn
  ) cpu.registers
  |> \reg -> { cpu | registers = reg }

