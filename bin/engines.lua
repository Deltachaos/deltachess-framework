#!/usr/bin/env lua
--[[
  Helper script to list all registered engines.
  Outputs engine IDs as JSON array for use in CI matrix.
  
  Usage:
    lua bin/engines.lua              -- outputs JSON array: ["engine1","engine2"]
    lua bin/engines.lua --check      -- exits 0 if engines found, 1 otherwise
]]

local source = debug.getinfo(1, "S").source
if source:sub(1, 1) == "@" then source = source:sub(2) end
local scriptDir = source:match("^(.*)[/\\][^/\\]+$") or "."
local sep = package.config:sub(1, 1)
local root = scriptDir .. sep .. ".."
local unitDirs = {
    root .. sep .. "Libs" .. sep .. "LibUnitWow",
    root .. sep .. ".." .. sep .. "LibUnitWow",
}
local unitDir
for i = 1, #unitDirs do
    local handle = io.open(unitDirs[i] .. sep .. "test.lua", "r")
    if handle then
        handle:close()
        unitDir = unitDirs[i]
        break
    end
end
if not unitDir then
    io.write("LibUnitWow not found\n")
    os.exit(1)
end
package.path = unitDir .. sep .. "?.lua;" .. package.path
require("test").loadToc(root .. sep .. "DeltachessFramework.toc")

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
