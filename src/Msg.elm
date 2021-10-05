module Msg exposing (..)


type Msg
    = UpdateScreen
    | Tick
    | InputPressed Int
    | InputReleased Int
      -- Chip 8 Msgs
    | Continue
    | FetchRandom Int Int
    | SetRandom Int
    | SetEmulatorRun Bool
    | Noop
      -- interop
    | FetchRom String
    | LoadRom (List Int)

