module Msg exposing (..)

type Msg
    = UpdateScreen
    | Tick
    | InputPressed Int
    | InputReleased Int
    | LoadRom String

    -- Chip 8 Msgs
    | Continue
    | FetchRandom Int Int
    | SetRandom Int
    | SetEmulatorRun Bool
    | Noop

