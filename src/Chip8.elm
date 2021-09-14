module Chip8 exposing (Cpu, defaultCpu, fps)
import Array exposing (Array)
import Bitwise exposing (shiftLeftBy, shiftRightBy, or, and)


-- constants
fps : Int
fps =
  60

opsPerSecond : Int
opsPerSecond =
  400


opsPerFrame : Int
opsPerFrame =
  opsPerSecond // fps


--This might have to be replaced with an actual byte
type alias Byte8 = Int


--2 Bytes, might have to replaced with something else
type alias Byte16 = Int


type alias Cpu =
  { memory : Array Byte8
  , screenBuffer : Array Bool

  -- registers
  , registers : Array Byte8
  , i : Byte16
  , pc : Byte16
  , stack : List Byte16

  -- timers
  , timerDelay : Byte8
  , timerSound : Byte8
  }


emptyMemory : Array Byte8
emptyMemory =
  List.repeat 0xFFF 0  |> Array.fromList


emptyBuffer : Array Bool
emptyBuffer =
  List.repeat (64*32) False |> Array.fromList


emptyRegisters : Array Byte8
emptyRegisters =
  List.repeat 0x010 0 |> Array.fromList


defaultCpu : Cpu
defaultCpu =
  { memory = emptyMemory
  , screenBuffer = emptyBuffer
  , registers = emptyRegisters
  , i = 0
  , pc = 0x200
  , stack = []
  , timerDelay = 0
  , timerSound = 0
  }


---- Bitwise helpers ----

overflow : Byte16 -> Byte8
overflow =
  and 0x100
  >> shiftRightBy 8

applyBitMask : Byte16 -> Byte16 -> Int
applyBitMask mask word =
  and mask word


get0NNN : Byte16 -> Int
get0NNN = 
  applyBitMask 0x0FFF


get0N00 : Byte16 -> Int
get0N00 =
  applyBitMask 0x0F00
  >> shiftRightBy 8


get00N0 : Byte16 -> Int
get00N0 =
  applyBitMask 0x00F0
  >> shiftRightBy 4


get000N : Byte16 -> Int
get000N =
  applyBitMask 0x000F


get00NN : Byte16 -> Int
get00NN =
  applyBitMask 0x00FF



getNextOp : Cpu -> Result String (Byte16, Cpu)
getNextOp cpu =
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


getOp : Byte16 -> Byte16 -> Cpu -> Cpu
getOp opcode =
  case and 0xF000 opcode of
    0x0000 ->
      case opcode of
      0x00E0 -> op_00E0
      0x00EE -> op_00EE
      _ -> op_0NNN
    0x1000 -> op_1NNN
    0x2000 -> op_2NNN
    0x3000 -> op_3XKK
    0x4000 -> op_4XKK
    0x5000 -> op_5XY0
    0x6000 -> op_6XKK
    0x7000 -> op_7XKK
    0x8000 ->
      case and 0x000F opcode of
      0x0000 -> op_8XY0
      0x0001 -> op_8XY1
      0x0002 -> op_8XY2
      0x0003 -> op_8XY3
      0x0004 -> op_8XY4
      0x0005 -> op_8XY5
      0x0006 -> op_8XY6
      0x0007 -> op_8XY7
      0x000E -> op_8XYE            
      _ -> noop
    0x9000 -> op_9XY0
    0xA000 -> op_ANNN
    0xB000 -> op_BNNN
    0xC000 -> op_CXKK
    0xD000 -> op_DXYN
    0xE000 ->
      case and 0x00FF opcode of
      0x009E -> op_EX9E
      0x00A1 -> op_EXA1
      _ -> noop
    0xF000 ->
      case and 0x00FF opcode of
      0x0007 -> op_FX07
      0x000A -> op_FX0A
      0x0015 -> op_FX15
      0x0018 -> op_FX18
      0x001E -> op_FX1E
      0x0029 -> op_FX29
      0x0033 -> op_FX33
      0x0055 -> op_FX55
      0x0065 -> op_FX65
      _ -> noop
    _ -> noop


doNextOp : Cpu -> Cpu
doNextOp prevcpu =
  case getNextOp prevcpu of
  Ok (opcode, cpu) ->
    getOp opcode
    |>(\op -> op opcode cpu)
  Err _ ->
    noop 0x0000 prevcpu


---- Op Code Functions ----

noop : Byte16 -> Cpu -> Cpu
noop _ cpu =
  cpu

-- 0nnn - SYS addr
-- Jump to a machine code routine at nnn.
op_0NNN : Byte16 -> Cpu -> Cpu
op_0NNN opcode cpu =
  {cpu | pc = get0NNN opcode}


-- 00E0 - CLS
-- Clear the display.
op_00E0 : Byte16 -> Cpu -> Cpu
op_00E0 _ cpu =
  { cpu | screenBuffer = emptyBuffer }


-- 00EE - RET
-- Return from a subroutine.
-- The interpreter sets the program counter to the address at the top of the stack, then subtracts 1 from the stack pointer.
op_00EE : Byte16 -> Cpu -> Cpu
op_00EE _ cpu =
  case cpu.stack of
  pc::stack ->
    { cpu
    | stack = stack
    , pc = pc
    }
  _ -> 
    cpu


-- 1nnn - JP addr
-- Jump to location nnn.
-- The interpreter sets the program counter to nnn.
op_1NNN : Byte16 -> Cpu -> Cpu
op_1NNN opcode cpu =
  {cpu | pc = get0NNN opcode}


-- 2nnn - CALL addr
-- Call subroutine at nnn.
-- The interpreter increments the stack pointer, then puts the current PC on the top of the stack. The PC is then set to nnn.
op_2NNN : Byte16 -> Cpu -> Cpu
op_2NNN opcode cpu =
  let
    pc =
      get0NNN opcode

    stack =
      cpu.pc::cpu.stack
  in
  { cpu
  | stack = stack
  , pc = pc
  }


-- 3xkk - SE Vx, byte
-- Skip next instruction if Vx = kk.
-- The interpreter compares register Vx to kk, and if they are equal, increments the program counter by 2.
op_3XKK : Byte16 -> Cpu -> Cpu
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
    { cpu | pc = cpu.pc + 2 }
  else
    cpu


-- 4xkk - SNE Vx, byte
-- Skip next instruction if Vx != kk.
-- The interpreter compares register Vx to kk, and if they are not equal, increments the program counter by 2.
op_4XKK : Byte16 -> Cpu -> Cpu
op_4XKK opcode cpu =
  let
    vx =
      cpu.registers
      |> Array.get ( get0N00 opcode )
      |> Maybe.withDefault -1

    kk =
      get00NN opcode
  in
  if vx /= kk then
    { cpu | pc = cpu.pc + 2 }
  else
    cpu


-- 5xy0 - SE Vx, Vy
-- Skip next instruction if Vx = Vy.
-- The interpreter compares register Vx to register Vy, and if they are equal, increments the program counter by 2.
op_5XY0 : Byte16 -> Cpu -> Cpu
op_5XY0 opcode cpu =
  let
    vx =
      cpu.registers
      |> Array.get ( get0N00 opcode )
      |> Maybe.withDefault -1

    vy =
      cpu.registers
      |> Array.get ( get00N0 opcode )
      |> Maybe.withDefault -1
  in
  if vx == vy then
    { cpu | pc = cpu.pc + 2 }
  else
    cpu


-- 6xkk - LD Vx, byte
-- Set Vx = kk.
-- The interpreter puts the value kk into register Vx.
op_6XKK : Byte16 -> Cpu -> Cpu
op_6XKK opcode cpu =
  let
    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) ( get00NN opcode ) 
  in
  { cpu | registers = registers }


-- 7xkk - ADD Vx, byte
-- Set Vx = Vx + kk.
-- Adds the value kk to the value of register Vx, then stores the result in Vx.
op_7XKK : Byte16 -> Cpu -> Cpu
op_7XKK opcode cpu =
  let
    vx =
      cpu.registers
      |> Array.get ( get0N00 opcode )
      |> Maybe.withDefault 0

    kk =
      get00NN opcode

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) ( vx + kk )
  in
  { cpu | registers = registers }

-- 8xy0 - LD Vx, Vy
-- Set Vx = Vy.
-- Stores the value of register Vy in register Vx.
op_8XY0 : Byte16 -> Cpu -> Cpu
op_8XY0 opcode cpu =
  let
    vy =
      cpu.registers
      |> Array.get ( get00N0 opcode )
      |> Maybe.withDefault 0

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) vy
  in
  { cpu | registers = registers }


-- 8xy1 - OR Vx, Vy
-- Set Vx = Vx OR Vy.
-- Performs a bitwise OR on the values of Vx and Vy, then stores the result in Vx. A bitwise OR compares the corrseponding bits from two values, and if either bit is 1, then the same bit in the result is also 1. Otherwise, it is 0.
op_8XY1 : Byte16 -> Cpu -> Cpu
op_8XY1 opcode cpu =
  let
    vx =
      cpu.registers
      |> Array.get ( get0N00 opcode )
      |> Maybe.withDefault 0

    vy =
      cpu.registers
      |> Array.get ( get00N0 opcode )
      |> Maybe.withDefault 0

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) ( or vx vy )

  in
  { cpu | registers = registers }



-- 8xy2 - AND Vx, Vy
-- Set Vx = Vx AND Vy.
-- Performs a bitwise AND on the values of Vx and Vy, then stores the result in Vx. A bitwise AND compares the corrseponding bits from two values, and if both bits are 1, then the same bit in the result is also 1. Otherwise, it is 0.
op_8XY2 : Byte16 -> Cpu -> Cpu
op_8XY2 opcode cpu =
  let
    vx =
      cpu.registers
      |> Array.get ( get0N00 opcode )
      |> Maybe.withDefault 0

    vy =
      cpu.registers
      |> Array.get ( get00N0 opcode )
      |> Maybe.withDefault 0

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) ( and vx vy )

  in
  { cpu | registers = registers }


-- 8xy3 - XOR Vx, Vy
-- Set Vx = Vx XOR Vy.
-- Performs a bitwise exclusive OR on the values of Vx and Vy, then stores the result in Vx. An exclusive OR compares the corrseponding bits from two values, and if the bits are not both the same, then the corresponding bit in the result is set to 1. Otherwise, it is 0.
op_8XY3 : Byte16 -> Cpu -> Cpu
op_8XY3 opcode cpu =
  let
    vx =
      cpu.registers
      |> Array.get ( get0N00 opcode )
      |> Maybe.withDefault 0

    vy =
      cpu.registers
      |> Array.get ( get00N0 opcode )
      |> Maybe.withDefault 0

    registers =
      cpu.registers
      |> Array.set ( get0N00 opcode ) ( Bitwise.xor vx vy )

  in
  { cpu | registers = registers }


-- 8xy4 - ADD Vx, Vy
-- Set Vx = Vx + Vy, set VF = carry.
-- The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. Only the lowest 8 bits of the result are kept, and stored in Vx.
op_8XY4 : Byte16 -> Cpu -> Cpu
op_8XY4 opcode cpu =
  cpu


-- 8xy5 - SUB Vx, Vy
-- Set Vx = Vx - Vy, set VF = NOT borrow.
-- If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from Vx, and the results stored in Vx.
op_8XY5 : Byte16 -> Cpu -> Cpu
op_8XY5 opcode cpu =
  cpu


-- 8xy6 - SHR Vx {, Vy}
-- Set Vx = Vx SHR 1.
-- If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. Then Vx is divided by 2.
op_8XY6 : Byte16 -> Cpu -> Cpu
op_8XY6 opcode cpu =
  cpu


-- 8xy7 - SUBN Vx, Vy
-- Set Vx = Vy - Vx, set VF = NOT borrow.
-- If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from Vy, and the results stored in Vx.
op_8XY7 : Byte16 -> Cpu -> Cpu
op_8XY7 opcode cpu =
  cpu


-- 8xyE - SHL Vx {, Vy}
-- Set Vx = Vx SHL 1.
-- If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0. Then Vx is multiplied by 2.
op_8XYE : Byte16 -> Cpu -> Cpu
op_8XYE opcode cpu =
  cpu


-- 9xy0 - SNE Vx, Vy
-- Skip next instruction if Vx != Vy.
-- The values of Vx and Vy are compared, and if they are not equal, the program counter is increased by 2.
op_9XY0 : Byte16 -> Cpu -> Cpu
op_9XY0 opcode cpu =
  cpu


-- Annn - LD I, addr
-- Set I = nnn.
-- The value of register I is set to nnn.
op_ANNN : Byte16 -> Cpu -> Cpu
op_ANNN opcode cpu =
  cpu


-- Bnnn - JP V0, addr
-- Jump to location nnn + V0.
-- The program counter is set to nnn plus the value of V0.
op_BNNN : Byte16 -> Cpu -> Cpu
op_BNNN opcode cpu =
  cpu


-- Cxkk - RND Vx, byte
-- Set Vx = random byte AND kk.
-- The interpreter generates a random number from 0 to 255, which is then ANDed with the value kk. The results are stored in Vx. See instruction 8xy2 for more information on AND.
op_CXKK : Byte16 -> Cpu -> Cpu
op_CXKK opcode cpu =
  cpu


-- Dxyn - DRW Vx, Vy, nibble
-- Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
-- The interpreter reads n bytes from memory, starting at the address stored in I. These bytes are then displayed as sprites on screen at coordinates (Vx, Vy). Sprites are XORed onto the existing screen. If this causes any pixels to be erased, VF is set to 1, otherwise it is set to 0. If the sprite is positioned so part of it is outside the coordinates of the display, it wraps around to the opposite side of the screen. See instruction 8xy3 for more information on XOR, and section 2.4, Display, for more information on the Chip-8 screen and sprites.
op_DXYN : Byte16 -> Cpu -> Cpu
op_DXYN opcode cpu =
  cpu


-- Ex9E - SKP Vx
-- Skip next instruction if key with the value of Vx is pressed.
-- Checks the keyboard, and if the key corresponding to the value of Vx is currently in the down position, PC is increased by 2.
op_EX9E : Byte16 -> Cpu -> Cpu
op_EX9E opcode cpu =
  cpu


-- ExA1 - SKNP Vx
-- Skip next instruction if key with the value of Vx is not pressed.
-- Checks the keyboard, and if the key corresponding to the value of Vx is currently in the up position, PC is increased by 2.
op_EXA1 : Byte16 -> Cpu -> Cpu
op_EXA1 opcode cpu =
  cpu


-- Fx07 - LD Vx, DT
-- Set Vx = delay timer value.
-- The value of DT is placed into Vx.
op_FX07 : Byte16 -> Cpu -> Cpu
op_FX07 opcode cpu =
  cpu


-- Fx0A - LD Vx, K
-- Wait for a key press, store the value of the key in Vx.
-- All execution stops until a key is pressed, then the value of that key is stored in Vx.
op_FX0A : Byte16 -> Cpu -> Cpu
op_FX0A opcode cpu =
  cpu


-- Fx15 - LD DT, Vx
-- Set delay timer = Vx.
-- DT is set equal to the value of Vx.
op_FX15 : Byte16 -> Cpu -> Cpu
op_FX15 opcode cpu =
  cpu


-- Fx18 - LD ST, Vx
-- Set sound timer = Vx.
-- ST is set equal to the value of Vx.
op_FX18 : Byte16 -> Cpu -> Cpu
op_FX18 opcode cpu =
  cpu


-- Fx1E - ADD I, Vx
-- Set I = I + Vx.
-- The values of I and Vx are added, and the results are stored in I.
op_FX1E : Byte16 -> Cpu -> Cpu
op_FX1E opcode cpu =
  cpu


-- Fx29 - LD F, Vx
-- Set I = location of sprite for digit Vx.
-- The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx. See section 2.4, Display, for more information on the Chip-8 hexadecimal font.
op_FX29 : Byte16 -> Cpu -> Cpu
op_FX29 opcode cpu =
  cpu


-- Fx33 - LD B, Vx
-- Store BCD representation of Vx in memory locations I, I+1, and I+2.
-- The interpreter takes the decimal value of Vx, and places the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2.
op_FX33 : Byte16 -> Cpu -> Cpu
op_FX33 opcode cpu =
  cpu


-- Fx55 - LD [I], Vx
-- Store registers V0 through Vx in memory starting at location I.
-- The interpreter copies the values of registers V0 through Vx into memory, starting at the address in I.
op_FX55 : Byte16 -> Cpu -> Cpu
op_FX55 opcode cpu =
  cpu


-- Fx65 - LD Vx, [I]
-- Read registers V0 through Vx from memory starting at location I.
-- The interpreter reads values from memory starting at location I into registers V0 through Vx.
op_FX65 : Byte16 -> Cpu -> Cpu
op_FX65 opcode cpu =
  cpu

