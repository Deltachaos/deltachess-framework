#!/usr/bin/env lua
--[[
  UCI (Universal Chess Interface) compatible interface for DeltaChess engines.
  
  Reads the engine ID from a command-line argument, the ENGINE environment
  variable, or interactively via the 'engine' command.
  
  Usage:
    lua bin/uci.lua fruit21           # specify engine as argument
    ENGINE=fruit21 lua bin/uci.lua    # specify via environment variable
    lua bin/uci.lua                   # select engine interactively
    
  UCI protocol commands supported:
    - engine <engine_id>: (custom) select engine interactively
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

-- Get engine ID from command-line argument or environment variable
local engineId = arg[1] or os.getenv("ENGINE")
local engine = nil
local engineName = nil
local engineAuthor = nil

-- Helper function to set/load engine
local function setEngine(id)
  local eng = DeltaChess.Engines:Get(id)
  if not eng then
    io.write("info string Error: Engine '" .. id .. "' not found\n")
    io.write("info string Available engines: ")
    local engineList = DeltaChess.Engines:GetEngineList()
    if engineList then
      for i, e in ipairs(engineList) do
        if i > 1 then io.write(", ") end
        io.write(e.id)
      end
    end
    io.write("\n")
    io.flush()
    return false
  end
  
  engineId = id
  engine = eng
  engineName = engine.name or engineId
  engineAuthor = engine.author or "Unknown"
  return true
end

-- Try to set engine if provided
if engineId and engineId ~= "" then
  if not setEngine(engineId) then
    os.exit(1)
  end
end

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
    if line ~= "" then
      local tokens = parseTokens(line)
      local cmd = tokens[1]
      
      if cmd == "engine" then
        -- Custom command to set engine
        if tokens[2] then
          if setEngine(tokens[2]) then
            io.write("uciok\n")
            io.flush()
          end
        else
          io.write("info string Usage: engine <engine_id>\n")
          io.flush()
        end
        
      elseif cmd == "uci" then
        if engine then
          io.write("id name " .. engineName .. " (DeltaChess)\n")
          io.write("id author " .. engineAuthor .. "\n")
        else
          io.write("id name DeltaChess UCI Interface\n")
          io.write("id author DeltaChess\n")
          io.write("info string No engine selected. Use: engine <engine_id>\n")
        end
        
        -- Report UCI options (none for now)
        
        io.write("uciok\n")
        io.flush()
        
      elseif cmd == "isready" then
        if engine then
          io.write("readyok\n")
        else
          io.write("info string No engine selected. Use: engine <engine_id>\n")
          io.write("readyok\n")
        end
        io.flush()
        
      elseif cmd == "ucinewgame" then
        if not engine then
          io.write("info string No engine selected. Use: engine <engine_id>\n")
          io.flush()
        else
          currentFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
          moveHistory = {}
        end
        
      elseif cmd == "position" then
        if not engine then
          io.write("info string No engine selected. Use: engine <engine_id>\n")
          io.flush()
        else
          handlePosition(tokens)
        end
        
      elseif cmd == "go" then
        if not engine then
          io.write("info string No engine selected. Use: engine <engine_id>\n")
          io.write("bestmove 0000\n")
          io.flush()
        else
          handleGo(tokens)
        end
        
      elseif cmd == "quit" then
        break
        
      elseif cmd == "stop" then
        -- Not implemented - engines run synchronously
        isCalculating = false
        
      else
        -- Unknown command, ignore per UCI spec
      end
    end
  end
end

-- Run the UCI loop
uciLoop()
