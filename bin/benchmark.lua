#!/usr/bin/env lua
--[[
  Benchmark: run every engine at all supported ELO settings (100 ELO steps)
  vs random moves. Play maximum of 4 engine moves per test; measure average
  thinking time per move. Every 10 tests print a ranking table (engine, elo,
  avg ms, time/elo) sorted by time/elo.
  Usage: lua bin/benchmark.lua
]]

local scriptPath = debug.getinfo(1, "S").source:match("@?(.*/)")
if not scriptPath then scriptPath = "./" end
local projectRoot = scriptPath .. "../"
package.path = projectRoot .. "src/?.lua;src/?/init.lua;lib/?.lua;?.lua;" .. (package.path or "")

DeltaChess = DeltaChess or {}
require("init")

local EngineRunner = DeltaChess.EngineRunner
local Engines = DeltaChess.Engines
local Constants = DeltaChess.Constants
local Board = DeltaChess.Board
local MoveGen = DeltaChess.MoveGen

local ELO_STEP = 100
local MAX_ENGINE_MOVES = 4

-- Build list of { engineId, elo } for every engine at 100 ELO steps (min to max)
local function buildTests()
  local tests = {}
  for id, engine in pairs(Engines.Registry or {}) do
    if type(engine.GetEloRange) == "function" then
      local ok, range = pcall(function() return engine:GetEloRange() end)
      if ok and range and range[1] and range[2] then
        local minElo, maxElo = range[1], range[2]
        for elo = minElo, maxElo, ELO_STEP do
          table.insert(tests, { engineId = id, elo = elo })
        end
      end
    end
  end
  return tests
end

-- Pick a random legal move from current board position. Returns UCI string or nil.
local function randomLegalMove(board)
  local legal = board:LegalMoves()
  if not legal or #legal == 0 then return nil end
  local idx = math.random(1, #legal)
  return MoveGen.MoveToUci(legal[idx])
end

-- Run one test: engine vs random, up to MAX_ENGINE_MOVES engine moves; return average thinking time in ms or nil on error.
-- Runs synchronously via Scheduler(next) => next().
local function runOneTest(engineId, elo)
  local board = Board.New()
  local times = {}   -- list of thinking times in seconds (from os.clock())
  local engineMoveCount = 0
  local done = false
  local errMsg = nil

  local function playRandomMove()
    local uci = randomLegalMove(board)
    if not uci then return true end  -- game over (no legal moves)
    local ok = board:MakeMoveUci(uci)
    if not ok then return true end
    return false
  end

  local function doEngineMove(cont)
    if done then if cont then cont() end return end
    if board:IsEnded() then
      done = true
      if cont then cont() end
      return
    end
    if engineMoveCount >= MAX_ENGINE_MOVES then
      done = true
      if cont then cont() end
      return
    end

    local fen = board:GetFen()
    local t0 = os.clock()

    EngineRunner.Create(engineId)
      :Fen(fen)
      :Elo(elo)
      :Scheduler(function(next) next() end)
      :OnComplete(function(res, err)
        local t1 = os.clock()
        if err or not res or not res.move then
          errMsg = err and (err.message or tostring(err)) or "no move"
          done = true
          if cont then cont() end
          return
        end
        local ok = board:MakeMoveUci(res.move)
        if not ok then
          errMsg = "illegal move"
          done = true
          if cont then cont() end
          return
        end
        engineMoveCount = engineMoveCount + 1
        table.insert(times, t1 - t0)

        if board:IsEnded() or engineMoveCount >= MAX_ENGINE_MOVES then
          done = true
          if cont then cont() end
          return
        end
        -- Random reply
        if playRandomMove() then
          done = true
          if cont then cont() end
          return
        end
        doEngineMove(cont)
      end)
      :Run()
  end

  -- Engine plays white (first move); runs synchronously via Scheduler(next) next()
  doEngineMove(function() end)

  if errMsg then return nil, errMsg end
  if #times == 0 then return nil, "no moves" end
  local sum = 0
  for _, t in ipairs(times) do sum = sum + t end
  return (sum / #times) * 1000  -- average ms
end

-- Results: { engineId, elo, avgTimeMs, timePerElo }
local results = {}

local function printRanking()
  local list = {}
  for _, r in ipairs(results) do
    list[#list + 1] = {
      engineId = r.engineId,
      elo = r.elo,
      avgTimeMs = r.avgTimeMs,
      timePerElo = r.timePerElo,
    }
  end
  table.sort(list, function(a, b) return a.timePerElo < b.timePerElo end)
  io.write("\n--- Ranking (time/elo, lower is better) ---\n")
  io.write(string.format("  %-24s %6s %12s %12s\n", "Engine", "ELO", "Avg(ms)", "Time/ELO"))
  io.write("  " .. string.rep("-", 56) .. "\n")
  for i, row in ipairs(list) do
    io.write(string.format("  %2d. %-20s %6d %12.2f %12.4f\n",
      i, row.engineId, row.elo, row.avgTimeMs, row.timePerElo))
  end
  io.write("\n")
end

local tests = buildTests()
if #tests == 0 then
  io.stderr:write("No engines with GetEloRange found.\n")
  os.exit(1)
end

math.randomseed(os.time())
io.write("Benchmark: " .. #tests .. " tests (each engine at 100 ELO steps, 4 moves vs random).\n\n")

for i, t in ipairs(tests) do
  io.write("[" .. i .. "/" .. #tests .. "] " .. t.engineId .. " @" .. t.elo .. " ... ")
  io.flush()
  local avgMs, err = runOneTest(t.engineId, t.elo)
  if not avgMs then
    io.write("FAIL (" .. tostring(err) .. ")\n")
  else
    local timePerElo = avgMs / t.elo
    table.insert(results, {
      engineId = t.engineId,
      elo = t.elo,
      avgTimeMs = avgMs,
      timePerElo = timePerElo,
    })
    io.write(string.format("%.2f ms avg\n", avgMs))
  end
  if i % 10 == 0 and #results > 0 then
    printRanking()
  end
end

if #results > 0 then
  io.write("Done. Final ")
  printRanking()
end
