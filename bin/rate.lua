#!/usr/bin/env lua
--[[
  Run engine-vs-engine benchmark: each registered engine at min and max ELO
  vs every other (engine, ELO) combination. One JSON snapshot per match in
  tests/integration/__snapshot__. After each match, print a ranking table.
  Usage: lua bin/rate.lua [-v|-vv] [max_half_moves]
  -v   print moves in SAN notation (e.g. 1. e4 Nc6 2. O-O)
  -vv  display board after each move (ANSI in-place update when possible)
]]

local scriptPath = debug.getinfo(1, "S").source:match("@?(.*/)")
if not scriptPath then scriptPath = "./" end
local projectRoot = scriptPath .. "../"
package.path = projectRoot .. "src/?.lua;src/?/init.lua;lib/?.lua;?.lua;" .. (package.path or "")

DeltaChess = DeltaChess or {}
require("init")

local EngineDuel = DeltaChess.EngineDuel
local Engines = DeltaChess.Engines
local Constants = DeltaChess.Constants
local Util = DeltaChess.Util
local MoveGen = DeltaChess.MoveGen

-- Parse -v / -vv and optional max_half_moves
local verboseLevel = 0  -- 0 = quiet, 1 = UCI moves, 2 = board in-place
local maxHalfMovesArg = 200
if arg then
  for i = 1, (arg.n or #arg) do
    local a = arg[i]
    if type(a) == "string" then
      if a == "-vv" then
        verboseLevel = 2
      elseif a == "-v" then
        verboseLevel = math.max(verboseLevel, 1)
      elseif a:match("^%d+$") then
        maxHalfMovesArg = tonumber(a) or maxHalfMovesArg
      end
    end
  end
end

local sep = package.config:sub(1, 1)
local SNAPSHOT_DIR = projectRoot:gsub("/", sep):gsub(sep .. "$", "") .. sep .. "tests" .. sep .. "integration" .. sep .. "__snapshot__"
local MAX_HALF_MOVES = maxHalfMovesArg

-- Build list of "sides": each engine at min ELO and at max ELO (2 per engine)
local function buildSides()
  local sides = {}
  for id, engine in pairs(Engines.Registry or {}) do
    if type(engine.GetEloRange) == "function" then
      local ok, range = pcall(function() return engine:GetEloRange() end)
      if ok and range and range[1] and range[2] then
        local minElo, maxElo = range[1], range[2]
        table.insert(sides, { engineId = id, elo = minElo, label = id .. "@" .. tostring(minElo) })
        if minElo ~= maxElo then
          table.insert(sides, { engineId = id, elo = maxElo, label = id .. "@" .. tostring(maxElo) })
        end
      end
    end
  end
  return sides
end

-- All match pairs: (white_side, black_side) for distinct sides (no self-play same side)
local function buildMatches(sides)
  local matches = {}
  for i, white in ipairs(sides) do
    for j, black in ipairs(sides) do
      if i ~= j then
        table.insert(matches, { white = white, black = black })
      end
    end
  end
  return matches
end

local function matchFilename(white, black)
  local w = (white.engineId .. "_" .. tostring(white.elo)):gsub("[^%w_]", "_")
  local b = (black.engineId .. "_" .. tostring(black.elo)):gsub("[^%w_]", "_")
  return w .. "_vs_" .. b .. ".json"
end

local function ensureDir(dir)
  if sep == "\\" then
    os.execute("if not exist \"" .. dir .. "\" mkdir \"" .. dir .. "\"")
  else
    os.execute("mkdir -p \"" .. dir .. "\" 2>/dev/null")
  end
end

-- Points: key = side label (engineId@elo), value = points (1 win, 0.5 draw, 0 loss)
local points = {}

local function sideKey(side)
  return side.engineId .. "@" .. tostring(side.elo)
end

local function addPoints(whiteSide, blackSide, result)
  local whitePts = (result == Constants.WHITE and 1) or (result == Constants.DRAWN or result == Constants.TIMEOUT) and 0.5 or 0
  local blackPts = (result == Constants.BLACK and 1) or (result == Constants.DRAWN or result == Constants.TIMEOUT) and 0.5 or 0
  points[sideKey(whiteSide)] = (points[sideKey(whiteSide)] or 0) + whitePts
  points[sideKey(blackSide)] = (points[sideKey(blackSide)] or 0) + blackPts
end

local function printRanking()
  local list = {}
  for label, pts in pairs(points) do
    list[#list + 1] = { label = label, points = pts }
  end
  table.sort(list, function(a, b) return a.points > b.points end)
  io.write("\n--- Ranking (points) ---\n")
  for i, row in ipairs(list) do
    io.write(string.format("  %2d. %-30s %6.1f\n", i, row.label, row.points))
  end
  io.write("\n")
end

-- Serialize record for JSON (positions 0-based: use array with "0" key or array 1-based and store [0] separately; JSON doesn't have 0-index arrays so use object)
local function recordToJson(r)
  local positionsArr = {}
  if r.positions then
    for i = 0, #(r.moves or {}) do
      if r.positions[i] then
        positionsArr[tostring(i)] = r.positions[i]
      end
    end
  end
  return {
    id = r.id,
    whiteEngine = r.whiteEngine,
    blackEngine = r.blackEngine,
    whiteElo = r.whiteElo,
    blackElo = r.blackElo,
    maxHalfMoves = r.maxHalfMoves,
    result = r.result,
    endReason = r.endReason or "",
    moves = r.moves or {},
    positions = (next(positionsArr) and positionsArr) or nil,
  }
end

local sides = buildSides()
if #sides == 0 then
  io.stderr:write("No engines with GetEloRange found.\n")
  os.exit(1)
end

local matches = buildMatches(sides)
io.write("Engines (each at min/max ELO): " .. #sides .. " sides, " .. #matches .. " matches.\n")
io.write("Snapshot dir: " .. SNAPSHOT_DIR .. "\n\n")
ensureDir(SNAPSHOT_DIR)

local matchIndex = 1

local function runNext()
  if matchIndex > #matches then
    io.write("Done. Final ")
    printRanking()
    os.exit(0)
    return
  end

  local m = matches[matchIndex]
  local white = m.white
  local black = m.black
  local fname = matchFilename(white, black)
  local filepath = SNAPSHOT_DIR .. sep .. fname

  if not Engines:Get(white.engineId) or not Engines:Get(black.engineId) then
    io.stderr:write("Skipping " .. fname .. ": engine not found\n")
    matchIndex = matchIndex + 1
    runNext()
    return
  end

  io.write("[" .. matchIndex .. "/" .. #matches .. "] " .. white.label .. " vs " .. black.label .. " ... ")
  io.flush()

  local positions = {}
  positions[0] = Constants.START_FEN
  local boardLineCount = 0   -- for -vv in-place update
  local firstMovePrinted = false

  EngineDuel.PlayGame(white.engineId, white.elo, black.engineId, black.elo, MAX_HALF_MOVES, {
    onState = function(state)
      if state.fen and state.halfMoveNum then
        positions[state.halfMoveNum] = state.fen
      end
      if verboseLevel >= 1 and state.lastMove then
        local san = state.lastMoveSan
        if not san and MoveGen and MoveGen.UciToSan then
          local uci = type(state.lastMove) == "string" and state.lastMove or tostring(state.lastMove)
          local prevFen = positions[(state.halfMoveNum or 1) - 1] or Constants.START_FEN
          local pos = MoveGen.ParseFen(prevFen)
          san = pos and MoveGen.UciToSan(pos, uci) or uci
        end
        if san and san ~= "" then
          if not firstMovePrinted then
            io.write("\n  ")
            firstMovePrinted = true
          end
          local half = state.halfMoveNum or 0
          local fullMove = math.ceil(half / 2)
          if half % 2 == 1 then
            io.write(fullMove .. ". " .. san .. " ")
          else
            io.write(san .. " ")
          end
          io.flush()
        end
      end
      if verboseLevel >= 2 and state.fen and Util and Util.FenToBoard then
        local boardStr = Util.FenToBoard(state.fen)
        if boardStr and boardStr ~= "" then
          if boardLineCount > 0 then
            io.write("\27[" .. boardLineCount .. "A")  -- cursor up to overwrite previous board
          else
            io.write("\n")
          end
          local n = (select(2, boardStr:gsub("\n", "\n")) or 0) + 1
          boardLineCount = n
          io.write(boardStr .. "\n")
          io.flush()
        end
      end
    end,
  }, function(result, moves, errorMsg)
    local n = #moves
    if result == Constants.ERROR then
      io.write("ERROR (" .. tostring(errorMsg and (errorMsg.message or errorMsg)) .. ")\n")
    else
      io.write(result .. " after " .. n .. " half-moves\n")
    end

    local board = DeltaChess.Board.New()
    for _, uci in ipairs(moves) do
      board:MakeMoveUci(uci)
    end
    local endReason = board:GetEndReason()

    addPoints(white, black, result)

    local record = {
      id = fname:gsub("%.json$", ""),
      whiteEngine = white.engineId,
      blackEngine = black.engineId,
      whiteElo = white.elo,
      blackElo = black.elo,
      maxHalfMoves = MAX_HALF_MOVES,
      result = result,
      endReason = endReason or "",
      moves = moves,
      positions = positions,
    }

    local jsonStr, err = Util.SerializeJSON(recordToJson(record))
    if not jsonStr then
      io.stderr:write("SerializeJSON failed: " .. tostring(err) .. "\n")
      runNext()
      return
    end
    local f = io.open(filepath, "w")
    if f then
      f:write(jsonStr)
      f:close()
    else
      io.stderr:write("Could not write " .. filepath .. "\n")
    end

    printRanking()
    matchIndex = matchIndex + 1
    runNext()
  end)
end

runNext()
