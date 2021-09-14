module Chip8 exposing (Cpu, defaultCpu, doNextOp, fps)
import Array exposing (Array)
import Bitwise exposing (shiftLeftBy, shiftRightBy, or, and)


-- constants
fps : Int
fps =
  60

updatesPerSecond : Int
updatesPerSecond =
  400



--This might have to be replaced with an actual byte
-- 8 bits
type alias Byte = Int


--2 Bytes, might have to replaced with something else
-- 16 bits
type alias Word = Int


type alias Cpu =
  { memory : Array Byte
  , screenBuffer : Array Bool

  -- registers
  , registers : Array Byte
  , i : Word
  , pc : Word
  , stack : List Word

  -- timers
  , timerDelay : Byte
  , timerSound : Byte
  }


defaultCpu : Cpu
defaultCpu =
  { memory = List.repeat 0xFFF 0  |> Array.fromList
  , screenBuffer = List.repeat (64*32) False |> Array.fromList
  , registers = List.repeat 0x010 0 |> Array.fromList
  , i = 0
  , pc = 0x200
  , stack = []
  , timerDelay = 0
  , timerSound = 0
  }


---- helpers ----

applyBitMask : Word -> Word -> Int
applyBitMask mask word =
  and mask word


get0NNN : Word -> Int
get0NNN = 
  applyBitMask 0x0FFF


get0N00 : Word -> Int
get0N00 =
  applyBitMask 0x0F00
  >> shiftRightBy 8


get00N0 : Word -> Int
get00N0 =
  applyBitMask 0x00F0
  >> shiftRightBy 4


get000N : Word -> Int
get000N =
  applyBitMask 0x000F


get00NN : Word -> Int
get00NN =
  applyBitMask 0x00FF



getNextOp : Cpu -> Result String (Word, Cpu)
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


doNextOp : Cpu -> Cpu
doNextOp prevcpu =
  let
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
        0x7000 -> op_6XKK
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
  in
  case getNextOp prevcpu of
  Ok (opcode, cpu) ->
    getOp opcode
    |>(\op -> op opcode cpu)
  Err _ ->
    noop 0x0000 prevcpu


---- Op Code Functions ----

noop : Word -> Cpu -> Cpu
noop _ cpu =
  cpu

-- 0nnn - SYS addr
-- Jump to a machine code routine at nnn.
op_0NNN : Word -> Cpu -> Cpu
op_0NNN opcode cpu =
  cpu


-- 00E0 - CLS
-- Clear the display.
op_00E0 : Word -> Cpu -> Cpu
op_00E0 opcode cpu =
  cpu


-- 00EE - RET
-- Return from a subroutine.
-- The interpreter sets the program counter to the address at the top of the stack, then subtracts 1 from the stack pointer.
op_00EE : Word -> Cpu -> Cpu
op_00EE opcode cpu =
  cpu


-- 1nnn - JP addr
-- Jump to location nnn.
-- The interpreter sets the program counter to nnn.
op_1NNN : Word -> Cpu -> Cpu
op_1NNN opcode cpu =
  cpu


-- 2nnn - CALL addr
-- Call subroutine at nnn.
-- The interpreter increments the stack pointer, then puts the current PC on the top of the stack. The PC is then set to nnn.
op_2NNN : Word -> Cpu -> Cpu
op_2NNN opcode cpu =
  cpu


-- 3xkk - SE Vx, byte
-- Skip next instruction if Vx = kk.
-- The interpreter compares register Vx to kk, and if they are equal, increments the program counter by 2.
op_3XKK : Word -> Cpu -> Cpu
op_3XKK opcode cpu =
  cpu


-- 4xkk - SNE Vx, byte
-- Skip next instruction if Vx != kk.
-- The interpreter compares register Vx to kk, and if they are not equal, increments the program counter by 2.
op_4XKK : Word -> Cpu -> Cpu
op_4XKK opcode cpu =
  cpu


-- 5xy0 - SE Vx, Vy
-- Skip next instruction if Vx = Vy.
-- The interpreter compares register Vx to register Vy, and if they are equal, increments the program counter by 2.
op_5XY0 : Word -> Cpu -> Cpu
op_5XY0 opcode cpu =
  cpu


-- 6xkk - LD Vx, byte
-- Set Vx = kk.
-- The interpreter puts the value kk into register Vx.
op_6XKK : Word -> Cpu -> Cpu
op_6XKK opcode cpu =
  cpu


-- 7xkk - ADD Vx, byte
-- Set Vx = Vx + kk.
-- Adds the value kk to the value of register Vx, then stores the result in Vx.
op_7XKK : Word -> Cpu -> Cpu
op_7XKK opcode cpu =
  cpu

-- 8xy0 - LD Vx, Vy
-- Set Vx = Vy.
-- Stores the value of register Vy in register Vx.
op_8XY0 : Word -> Cpu -> Cpu
op_8XY0 opcode cpu =
  cpu


-- 8xy1 - OR Vx, Vy
-- Set Vx = Vx OR Vy.
-- Performs a bitwise OR on the values of Vx and Vy, then stores the result in Vx. A bitwise OR compares the corrseponding bits from two values, and if either bit is 1, then the same bit in the result is also 1. Otherwise, it is 0.
op_8XY1 : Word -> Cpu -> Cpu
op_8XY1 opcode cpu =
  cpu



-- 8xy2 - AND Vx, Vy
-- Set Vx = Vx AND Vy.
-- Performs a bitwise AND on the values of Vx and Vy, then stores the result in Vx. A bitwise AND compares the corrseponding bits from two values, and if both bits are 1, then the same bit in the result is also 1. Otherwise, it is 0.
op_8XY2 : Word -> Cpu -> Cpu
op_8XY2 opcode cpu =
  cpu


-- 8xy3 - XOR Vx, Vy
-- Set Vx = Vx XOR Vy.
-- Performs a bitwise exclusive OR on the values of Vx and Vy, then stores the result in Vx. An exclusive OR compares the corrseponding bits from two values, and if the bits are not both the same, then the corresponding bit in the result is set to 1. Otherwise, it is 0.
op_8XY3 : Word -> Cpu -> Cpu
op_8XY3 opcode cpu =
  cpu


-- 8xy4 - ADD Vx, Vy
-- Set Vx = Vx + Vy, set VF = carry.
-- The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. Only the lowest 8 bits of the result are kept, and stored in Vx.
op_8XY4 : Word -> Cpu -> Cpu
op_8XY4 opcode cpu =
  cpu


-- 8xy5 - SUB Vx, Vy
-- Set Vx = Vx - Vy, set VF = NOT borrow.
-- If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted from Vx, and the results stored in Vx.
op_8XY5 : Word -> Cpu -> Cpu
op_8XY5 opcode cpu =
  cpu


-- 8xy6 - SHR Vx {, Vy}
-- Set Vx = Vx SHR 1.
-- If the least-significant bit of Vx is 1, then VF is set to 1, otherwise 0. Then Vx is divided by 2.
op_8XY6 : Word -> Cpu -> Cpu
op_8XY6 opcode cpu =
  cpu


-- 8xy7 - SUBN Vx, Vy
-- Set Vx = Vy - Vx, set VF = NOT borrow.
-- If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted from Vy, and the results stored in Vx.
op_8XY7 : Word -> Cpu -> Cpu
op_8XY7 opcode cpu =
  cpu


-- 8xyE - SHL Vx {, Vy}
-- Set Vx = Vx SHL 1.
-- If the most-significant bit of Vx is 1, then VF is set to 1, otherwise to 0. Then Vx is multiplied by 2.
op_8XYE : Word -> Cpu -> Cpu
op_8XYE opcode cpu =
  cpu


-- 9xy0 - SNE Vx, Vy
-- Skip next instruction if Vx != Vy.
-- The values of Vx and Vy are compared, and if they are not equal, the program counter is increased by 2.
op_9XY0 : Word -> Cpu -> Cpu
op_9XY0 opcode cpu =
  cpu


-- Annn - LD I, addr
-- Set I = nnn.
-- The value of register I is set to nnn.
op_ANNN : Word -> Cpu -> Cpu
op_ANNN opcode cpu =
  cpu


-- Bnnn - JP V0, addr
-- Jump to location nnn + V0.
-- The program counter is set to nnn plus the value of V0.
op_BNNN : Word -> Cpu -> Cpu
op_BNNN opcode cpu =
  cpu


-- Cxkk - RND Vx, byte
-- Set Vx = random byte AND kk.
-- The interpreter generates a random number from 0 to 255, which is then ANDed with the value kk. The results are stored in Vx. See instruction 8xy2 for more information on AND.
op_CXKK : Word -> Cpu -> Cpu
op_CXKK opcode cpu =
  cpu


-- Dxyn - DRW Vx, Vy, nibble
-- Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision.
-- The interpreter reads n bytes from memory, starting at the address stored in I. These bytes are then displayed as sprites on screen at coordinates (Vx, Vy). Sprites are XORed onto the existing screen. If this causes any pixels to be erased, VF is set to 1, otherwise it is set to 0. If the sprite is positioned so part of it is outside the coordinates of the display, it wraps around to the opposite side of the screen. See instruction 8xy3 for more information on XOR, and section 2.4, Display, for more information on the Chip-8 screen and sprites.
op_DXYN : Word -> Cpu -> Cpu
op_DXYN opcode cpu =
  cpu


-- Ex9E - SKP Vx
-- Skip next instruction if key with the value of Vx is pressed.
-- Checks the keyboard, and if the key corresponding to the value of Vx is currently in the down position, PC is increased by 2.
op_EX9E : Word -> Cpu -> Cpu
op_EX9E opcode cpu =
  cpu


-- ExA1 - SKNP Vx
-- Skip next instruction if key with the value of Vx is not pressed.
-- Checks the keyboard, and if the key corresponding to the value of Vx is currently in the up position, PC is increased by 2.
op_EXA1 : Word -> Cpu -> Cpu
op_EXA1 opcode cpu =
  cpu


-- Fx07 - LD Vx, DT
-- Set Vx = delay timer value.
-- The value of DT is placed into Vx.
op_FX07 : Word -> Cpu -> Cpu
op_FX07 opcode cpu =
  cpu


-- Fx0A - LD Vx, K
-- Wait for a key press, store the value of the key in Vx.
-- All execution stops until a key is pressed, then the value of that key is stored in Vx.
op_FX0A : Word -> Cpu -> Cpu
op_FX0A opcode cpu =
  cpu


-- Fx15 - LD DT, Vx
-- Set delay timer = Vx.
-- DT is set equal to the value of Vx.
op_FX15 : Word -> Cpu -> Cpu
op_FX15 opcode cpu =
  cpu


-- Fx18 - LD ST, Vx
-- Set sound timer = Vx.
-- ST is set equal to the value of Vx.
op_FX18 : Word -> Cpu -> Cpu
op_FX18 opcode cpu =
  cpu


-- Fx1E - ADD I, Vx
-- Set I = I + Vx.
-- The values of I and Vx are added, and the results are stored in I.
op_FX1E : Word -> Cpu -> Cpu
op_FX1E opcode cpu =
  cpu


-- Fx29 - LD F, Vx
-- Set I = location of sprite for digit Vx.
-- The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx. See section 2.4, Display, for more information on the Chip-8 hexadecimal font.
op_FX29 : Word -> Cpu -> Cpu
op_FX29 opcode cpu =
  cpu


-- Fx33 - LD B, Vx
-- Store BCD representation of Vx in memory locations I, I+1, and I+2.
-- The interpreter takes the decimal value of Vx, and places the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2.
op_FX33 : Word -> Cpu -> Cpu
op_FX33 opcode cpu =
  cpu


-- Fx55 - LD [I], Vx
-- Store registers V0 through Vx in memory starting at location I.
-- The interpreter copies the values of registers V0 through Vx into memory, starting at the address in I.
op_FX55 : Word -> Cpu -> Cpu
op_FX55 opcode cpu =
  cpu


-- Fx65 - LD Vx, [I]
-- Read registers V0 through Vx from memory starting at location I.
-- The interpreter reads values from memory starting at location I into registers V0 through Vx.
op_FX65 : Word -> Cpu -> Cpu
op_FX65 opcode cpu =
  cpu

