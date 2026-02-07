#!/usr/bin/env lua
--[[
  Helper script to list all registered engines.
  Outputs engine IDs as JSON array for use in CI matrix.
  
  Usage:
    lua bin/engines.lua              -- outputs JSON array: ["engine1","engine2"]
    lua bin/engines.lua --check      -- exits 0 if engines found, 1 otherwise
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

-- Check command line arguments
local checkMode = false
for _, a in ipairs(arg) do
  if a == "--check" then
    checkMode = true
  end
end

-- Get engine list
local engines = {}
if DeltaChess and DeltaChess.Engines then
  local list = DeltaChess.Engines:GetEngineList()
  if list then
    for _, engine in ipairs(list) do
      if engine.id then
        table.insert(engines, engine.id)
      end
    end
  end
end

-- Sort engines alphabetically
table.sort(engines)

if checkMode then
  -- Check mode: exit 0 if engines found, 1 otherwise
  if #engines > 0 then
    io.stderr:write("Found " .. #engines .. " engine(s): " .. table.concat(engines, ", ") .. "\n")
    os.exit(0)
  else
    io.stderr:write("No engines found\n")
    os.exit(1)
  end
end

-- Output JSON array
io.write("[")
for i, engineId in ipairs(engines) do
  if i > 1 then io.write(",") end
  -- Escape quotes in engine ID (though unlikely to have them)
  local escaped = engineId:gsub('"', '\\"')
  io.write('"' .. escaped .. '"')
end
io.write("]")
io.write("\n")
