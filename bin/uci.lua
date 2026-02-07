#!/usr/bin/env lua
--[[
  UCI (Universal Chess Interface) compatible interface for DeltaChess engines.
  
  Reads the engine ID from the ENGINE environment variable and provides a UCI
  interface to communicate with it.
  
  Usage:
    ENGINE=fruit21 lua bin/uci.lua
    
  UCI protocol commands supported:
    - uci: identify as UCI engine
    - isready: respond with readyok
    - ucinewgame: reset state
    - position [fen <fenstring> | startpos] [moves <move1> ... <moveN>]
    - go [wtime <x>] [btime <x>] [movetime <x>] [depth <x>] [nodes <x>]
    - quit: exit
]]

-- Get script directory and set up paths
local scriptPath = debug.getinfo(1, "S").source:match("^@?(.*)[/\\]")
if not scriptPath or scriptPath == "" then scriptPath = "." end
scriptPath = scriptPath:gsub("/", package.config:sub(1, 1))

local sep = package.config:sub(1, 1)
local projectRoot = scriptPath .. sep .. ".." .. sep

-- Set up package paths
package.path = projectRoot .. "lib" .. sep .. "?.lua;" ..
               projectRoot .. "src/?.lua;" ..
               projectRoot .. "tests/?.lua;" ..
               package.path

-- Load DeltaChess modules
require("init")

-- Get engine ID from environment variable
local engineId = os.getenv("ENGINE")
if not engineId or engineId == "" then
  io.stderr:write("Error: ENGINE environment variable not set\n")
  io.stderr:write("Usage: ENGINE=<engine_id> lua bin/uci.lua\n")
  os.exit(1)
end

-- Verify engine exists
local engine = DeltaChess.Engines:Get(engineId)
if not engine then
  io.stderr:write("Error: Engine '" .. engineId .. "' not found\n")
  io.stderr:write("Available engines: ")
  local engineList = DeltaChess.Engines:GetEngineList()
  if engineList then
    for i, e in ipairs(engineList) do
      if i > 1 then io.stderr:write(", ") end
      io.stderr:write(e.id)
    end
    io.stderr:write("\n")
  end
  os.exit(1)
end

-- Engine metadata
local engineName = engine.name or engineId
local engineAuthor = engine.author or "Unknown"

-- Current position state
local currentFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
local moveHistory = {}

-- Flag to track if engine is calculating
local isCalculating = false

-- Parse UCI command tokens
local function parseTokens(line)
  local tokens = {}
  for token in line:gmatch("%S+") do
    table.insert(tokens, token)
  end
  return tokens
end

-- Handle position command
local function handlePosition(tokens)
  local idx = 2
  moveHistory = {}
  
  if tokens[idx] == "startpos" then
    currentFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    idx = idx + 1
  elseif tokens[idx] == "fen" then
    -- Collect FEN string (6 components)
    local fenParts = {}
    idx = idx + 1
    for i = 1, 6 do
      if tokens[idx] then
        table.insert(fenParts, tokens[idx])
        idx = idx + 1
      end
    end
    currentFen = table.concat(fenParts, " ")
  end
  
  -- Parse moves if present
  if tokens[idx] == "moves" then
    idx = idx + 1
    while tokens[idx] do
      table.insert(moveHistory, tokens[idx])
      idx = idx + 1
    end
  end
end

-- Handle go command
local function handleGo(tokens)
  if isCalculating then
    return
  end
  
  isCalculating = true
  
  -- Parse go parameters
  local timeLimit = nil
  local depth = nil
  local nodes = nil
  
  local idx = 2
  while tokens[idx] do
    local cmd = tokens[idx]
    if cmd == "movetime" then
      timeLimit = tonumber(tokens[idx + 1])
      idx = idx + 2
    elseif cmd == "depth" then
      depth = tonumber(tokens[idx + 1])
      idx = idx + 2
    elseif cmd == "nodes" then
      nodes = tonumber(tokens[idx + 1])
      idx = idx + 2
    elseif cmd == "wtime" or cmd == "btime" or cmd == "winc" or cmd == "binc" or 
           cmd == "movestogo" or cmd == "infinite" then
      -- Skip unsupported parameters
      if cmd ~= "infinite" then
        idx = idx + 2
      else
        idx = idx + 1
      end
    else
      idx = idx + 1
    end
  end
  
  -- Build engine runner
  local runner = DeltaChess.EngineRunner.Create(engineId)
  
  -- Set position
  if #moveHistory > 0 then
    runner:Moves(moveHistory)
  else
    runner:Fen(currentFen)
  end
  
  -- Set limits
  if depth then
    runner:PlyLimit(depth)
  end
  
  if nodes then
    runner:NodeLimit(nodes)
  end
  
  if timeLimit then
    runner:TimeLimitMs(timeLimit)
  end
  
  -- Set completion callback
  runner:OnComplete(function(result, err)
    isCalculating = false
    
    if err then
      -- Output info about error (UCI allows info strings)
      local errMsg = type(err) == "table" and err.message or tostring(err)
      io.write("info string Error: " .. errMsg .. "\n")
      io.flush()
      
      -- Try to return a fallback move if available in result
      if result and result.move then
        io.write("bestmove " .. result.move .. "\n")
        io.flush()
      else
        -- No move available - send null move (0000)
        io.write("bestmove 0000\n")
        io.flush()
      end
      return
    end
    
    if not result or not result.move then
      io.write("info string Error: No move returned\n")
      io.write("bestmove 0000\n")
      io.flush()
      return
    end
    
    -- Output best move
    local bestmove = result.move
    io.write("bestmove " .. bestmove .. "\n")
    io.flush()
  end)
  
  -- Run the calculation
  runner:Run()
end

-- Main UCI loop
local function uciLoop()
  io.write("DeltaChess UCI Interface\n")
  io.flush()
  
  while true do
    local line = io.read("*l")
    if not line then
      break
    end
    
    line = line:match("^%s*(.-)%s*$") -- trim whitespace
    if line == "" then
      goto continue
    end
    
    local tokens = parseTokens(line)
    local cmd = tokens[1]
    
    if cmd == "uci" then
      io.write("id name " .. engineName .. " (DeltaChess)\n")
      io.write("id author " .. engineAuthor .. "\n")
      
      -- Report UCI options (none for now)
      
      io.write("uciok\n")
      io.flush()
      
    elseif cmd == "isready" then
      io.write("readyok\n")
      io.flush()
      
    elseif cmd == "ucinewgame" then
      currentFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      moveHistory = {}
      
    elseif cmd == "position" then
      handlePosition(tokens)
      
    elseif cmd == "go" then
      handleGo(tokens)
      
    elseif cmd == "quit" then
      break
      
    elseif cmd == "stop" then
      -- Not implemented - engines run synchronously
      isCalculating = false
      
    else
      -- Unknown command, ignore per UCI spec
    end
    
    ::continue::
  end
end

-- Run the UCI loop
uciLoop()
