#!/usr/bin/env lua

local scriptPath = debug.getinfo(1, "S").source:match("@?(.*/)")
if not scriptPath then scriptPath = "./" end
local projectRoot = scriptPath .. "../"
package.path = projectRoot .. "src/?.lua;src/?/init.lua;lib/?.lua;?.lua;" .. (package.path or "")
DeltaChess = DeltaChess or {}
require("init")

local Engines = DeltaChess.Engines
local EngineDuel = DeltaChess.EngineDuel
local Util = DeltaChess.Util

local DEFAULT_MAX_HALF_MOVES = EngineDuel.DEFAULT_MAX_HALF_MOVES

--------------------------------------------------------------------------------
-- CLI: parse args and run one game
--------------------------------------------------------------------------------
local function usage()
    io.stderr:write("Usage: lua bin/play.lua [-v|--verbose] [-m|--max-moves N] <engine1> [elo1] [engine2] [elo2]\n")
    io.stderr:write("  -v, --verbose     Print position (FEN) after every move.\n")
    io.stderr:write("  -vv                Print position as Unicode board after every move.\n")
    io.stderr:write("  -m, --max-moves N  Draw after N half-moves (default 200).\n")
    io.stderr:write("  engine1           Engine id for White (required).\n")
    io.stderr:write("  elo1             Optional ELO for White.\n")
    io.stderr:write("  engine2           Engine id for Black (default: engine1 = self-play).\n")
    io.stderr:write("  elo2             Optional ELO for Black.\n")
    io.stderr:write("Examples:\n")
    io.stderr:write("  lua bin/play.lua beat_highest_piece\n")
    io.stderr:write("  lua bin/play.lua -v -m 100 beat_highest_piece 500\n")
    io.stderr:write("  lua bin/play.lua -vv -m 4 beat_highest_piece\n")
    io.stderr:write("  lua bin/play.lua beat_highest_piece 500 other_engine 600\n")
end

local verboseLevel = 0  -- 0 = off, 1 = FEN, 2 = board view
local maxHalfMoves = DEFAULT_MAX_HALF_MOVES
local args = {}
if arg then
    local i = 1
    while i <= #arg do
        local a = arg[i]
        if a == "-vv" then
            verboseLevel = 2
            i = i + 1
        elseif a == "-v" or a == "--verbose" then
            verboseLevel = math.min(2, verboseLevel + 1)
            i = i + 1
        elseif (a == "-m" or a == "--max-moves") and arg[i + 1] then
            maxHalfMoves = tonumber(arg[i + 1]) or maxHalfMoves
            i = i + 2
        else
            args[#args + 1] = a
            i = i + 1
        end
    end
end

local engine1 = args[1]
if not engine1 or engine1 == "" then
    usage()
    os.exit(1)
end

local elo1, engine2, elo2 = nil, nil, nil
if args[2] then
    local n2 = tonumber(args[2])
    if n2 then
        elo1 = n2
        engine2 = args[3]
        elo2 = args[4] and tonumber(args[4])
    else
        engine2 = args[2]
        elo2 = args[3] and tonumber(args[3])
    end
end
if not engine2 or engine2 == "" then
    engine2 = engine1
end

if not Engines:Get(engine1) then
    io.stderr:write("Error: engine not found: " .. tostring(engine1) .. "\n")
    os.exit(1)
end
if not Engines:Get(engine2) then
    io.stderr:write("Error: engine not found: " .. tostring(engine2) .. "\n")
    os.exit(1)
end

local function movesToPairs(moves)
    local pairs = {}
    for i = 1, #moves, 2 do
        if moves[i + 1] then
            pairs[#pairs + 1] = { moves[i], moves[i + 1] }
        else
            pairs[#pairs + 1] = { moves[i] }
        end
    end
    return pairs
end

local function sideStr(engine, elo)
    return engine .. (elo and (" @" .. elo) or "")
end

local function printResultLine(result, moves, errorMsg)
    local n = #moves
    local movesStr = (n == 0) and "0 half-moves" or (n .. " half-move" .. (n == 1 and "" or "s"))
    if result == EngineDuel.WHITE then
        io.write(string.format("White (%s) wins after %s (1-0)\n", sideStr(engine1, elo1), movesStr))
    elseif result == EngineDuel.BLACK then
        io.write(string.format("Black (%s) wins after %s (0-1)\n", sideStr(engine2, elo2), movesStr))
    elseif result == EngineDuel.DRAWN then
        io.write(string.format("Draw (1/2-1/2) after %s\n", movesStr))
    elseif result == EngineDuel.TIMEOUT then
        io.write(string.format("Timeout (1/2-1/2) after %s\n", movesStr))
    elseif result == EngineDuel.ERROR then
        -- Determine which engine caused the error (n moves played = next was n+1, odd = White, even = Black)
        local isWhiteTurn = (n % 2) == 0
        local faultyEngine = isWhiteTurn and sideStr(engine1, elo1) or sideStr(engine2, elo2)
        local side = isWhiteTurn and "White" or "Black"
        local errStr = type(errorMsg) == "table" and Util.Dump(errorMsg) or tostring(errorMsg)
        io.write(string.format("Error by %s (%s) after %s: %s\n", side, faultyEngine, movesStr, errStr))
    else
        io.write("Result: " .. tostring(result) .. "\n")
    end
end

local function printGame(result, moves, errorMsg)
    local pairs = movesToPairs(moves)
    local fullMove = 1
    for _, pair in ipairs(pairs) do
        if pair[2] then
            io.write(string.format("%d. %s %s ", fullMove, pair[1], pair[2]))
            fullMove = fullMove + 1
        else
            io.write(string.format("%d. %s ", fullMove, pair[1]))
        end
    end
    io.write("\n")
    printResultLine(result, moves, errorMsg)
end

io.write(string.format("White: %s%s  Black: %s%s  Max moves: %d\n",
    engine1, elo1 and (" @" .. elo1) or "",
    engine2, elo2 and (" @" .. elo2) or "",
    maxHalfMoves))
io.write("Moves: ")

EngineDuel.PlayGame(engine1, elo1, engine2, elo2, maxHalfMoves, {
    onState = (verboseLevel >= 1) and function(state)
        if verboseLevel == 1 then
            io.write(state.fen .. "\n")
        else
            io.write(Util.FenToBoard(state.fen) .. "\n")
        end
    end,
}, function(result, moves, errorMsg)
    printGame(result, moves, errorMsg)
    os.exit(0)
end)
