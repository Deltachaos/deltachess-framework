#!/usr/bin/env lua
--[[
  Test runner for DeltaChess.
  Discovers and runs all tests in subdirectories of tests/.
  Supports parallelization: set TEST_WORKERS=N to run N worker processes;
  each worker runs only tests where (testIndex % N == workerId).

  Usage:
    lua bin/test.lua
    TEST_WORKERS=4 lua bin/test.lua   -- spawn 4 parallel workers (main spawns, then workers run slice)
    lua bin/test.lua --lua=C:\path\to\lua.exe
    set LUA_BIN=C:\path\to\lua.exe && lua bin/test.lua
    lua bin/test.lua --only=slug1,slug2   -- run only tests with these slugs
    lua bin/test.lua --ignore=slug1,slug2 -- skip tests with these slugs
]]

-- Parse command-line arguments
local luaBin = os.getenv("LUA_BIN") or os.getenv("LUA") or "lua"
local onlySlugs = {}
local ignoreSlugs = {}

for _, a in ipairs(arg) do
  local luaPath = a:match("^%-%-lua=(.+)$")
  if luaPath then
    luaBin = luaPath
  end
  
  local only = a:match("^%-%-only=(.+)$")
  if only then
    for slug in only:gmatch("[^,]+") do
      local trimmed = slug:gsub("^%s+", ""):gsub("%s+$", "")
      if trimmed ~= "" then
        table.insert(onlySlugs, trimmed)
      end
    end
  end
  
  local ignore = a:match("^%-%-ignore=(.+)$")
  if ignore then
    for slug in ignore:gmatch("[^,]+") do
      local trimmed = slug:gsub("^%s+", ""):gsub("%s+$", "")
      if trimmed ~= "" then
        table.insert(ignoreSlugs, trimmed)
      end
    end
  end
end

-- Get script directory (support both / and \ so Windows paths work)
local scriptPath = debug.getinfo(1, "S").source:match("^@?(.*)[/\\]")
if not scriptPath or scriptPath == "" then scriptPath = "." end
scriptPath = scriptPath:gsub("/", package.config:sub(1, 1))

-- Navigate to project root (bin/../) and resolve to absolute so workers find modules regardless of cwd
local sep = package.config:sub(1, 1)
local projectRoot = scriptPath .. sep .. ".." .. sep
local function isAbsolute(p)
  if p:find("^%a:[/\\]") then return true end  -- Windows drive
  if p:find("^[/\\]") then return true end      -- Unix or Windows root
  return false
end
if not isAbsolute(projectRoot) then
  local cwd
  if sep == "\\" then
    local f = io.popen("cd")
    if f then cwd = (f:read("a") or ""):gsub("\r?\n", ""); f:close() end
  else
    local f = io.popen("pwd")
    if f then cwd = (f:read("a") or ""):gsub("\r?\n", ""); f:close() end
  end
  if cwd and cwd ~= "" then
    projectRoot = cwd .. sep .. projectRoot
  end
end

local isWindows = (sep == "\\")
--- Return a string safe to pass as one shell argument (for os.execute).
--- Windows: double-quote wrapped, internal " doubled. Unix: double-quote wrapped, \ and " escaped.
local function escapeshellarg(s)
  if s == nil then return '""' end
  s = tostring(s)
  if isWindows then
    return '"' .. s:gsub('"', '""') .. '"'
  end
  return '"' .. s:gsub("\\", "\\\\"):gsub('"', '\\"') .. '"'
end
--- For Windows only: use inside cmd /c " ... " so the outer quote is not closed (each " becomes "").
local function escapeshellargCmdInner(s)
  if s == nil then return '""""' end
  s = tostring(s)
  return '""' .. s:gsub('"', '""') .. '""'
end
--- For embedding a value inside double quotes in a written shell script (escape \ and ").
local function forShDoubleQuotedValue(s)
  if s == nil then return "" end
  s = tostring(s)
  return s:gsub("\\", "\\\\"):gsub('"', '\\"')
end

-- Set up package paths (include lib/ so Util can load json polyfill before init)
package.path = projectRoot .. "lib" .. sep .. "?.lua;" ..
               projectRoot .. "src/?.lua;" ..
               projectRoot .. "tests/?.lua;" ..
               package.path

-- Load DeltaChess modules
require("init")

-- Load test framework
local Test = require("test")

-- Apply slug filters from command-line arguments or environment variables
local onlySlugsEnv = os.getenv("TEST_ONLY_SLUGS")
local ignoreSlugsEnv = os.getenv("TEST_IGNORE_SLUGS")

if onlySlugsEnv and onlySlugsEnv ~= "" then
  for slug in onlySlugsEnv:gmatch("[^,]+") do
    local trimmed = slug:gsub("^%s+", ""):gsub("%s+$", "")
    if trimmed ~= "" then
      table.insert(Test.onlySlugs, trimmed)
    end
  end
elseif #onlySlugs > 0 then
  Test.onlySlugs = onlySlugs
end

if ignoreSlugsEnv and ignoreSlugsEnv ~= "" then
  for slug in ignoreSlugsEnv:gmatch("[^,]+") do
    local trimmed = slug:gsub("^%s+", ""):gsub("%s+$", "")
    if trimmed ~= "" then
      table.insert(Test.ignoreSlugs, trimmed)
    end
  end
elseif #ignoreSlugs > 0 then
  Test.ignoreSlugs = ignoreSlugs
end

-- Debug output for slug filters
if #Test.onlySlugs > 0 then
  print("Only running tests with slugs: " .. table.concat(Test.onlySlugs, ", "))
end
if #Test.ignoreSlugs > 0 then
  print("Ignoring tests with slugs: " .. table.concat(Test.ignoreSlugs, ", "))
end

-- Cross-platform: list immediate subdirectories (no find on Windows)
local function discoverTestDirs(testsRoot)
  local dirs = {}
  local pathForShell = testsRoot:gsub("/", sep)

  local handle
  if sep == "\\" then
    handle = io.popen("dir /b /ad " .. escapeshellarg(pathForShell) .. " 2>nul")
  else
    handle = io.popen("find " .. escapeshellarg(pathForShell) .. " -mindepth 1 -maxdepth 1 -type d 2>/dev/null | sort")
  end
  if handle then
    for line in handle:lines() do
      local trimmed = line:gsub("^%s+", ""):gsub("%s+$", ""):gsub("\r", "")
      if trimmed ~= "" then
        -- find returns full paths; dir /b returns names only
        local full = isAbsolute(trimmed) and trimmed or (pathForShell .. sep .. trimmed)
        table.insert(dirs, full)
      end
    end
    handle:close()
  end
  if sep == "\\" and #dirs > 0 then
    table.sort(dirs)
  end
  return dirs
end

local testsRoot = projectRoot .. "tests"
local testDirs = discoverTestDirs(testsRoot)

if #testDirs == 0 then
  print("No test directories found in " .. testsRoot)
  os.exit(1)
end

-- Default worker count to number of CPU cores
local function getCpuCount()
  local n
  if sep == "\\" then
    n = tonumber(os.getenv("NUMBER_OF_PROCESSORS"))
    if not n then
      local f = io.popen("wmic cpu get NumberOfLogicalProcessors 2>nul")
      if f then
        local line = f:read("*l")
        line = f:read("*l")
        if line then n = tonumber(line:match("%d+")) end
        f:close()
      end
    end
  else
    local f = io.popen("nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null")
    if f then
      local s = (f:read("a") or ""):match("%d+")
      n = s and tonumber(s) or nil
      f:close()
    end
  end
  return (n and n > 0) and n or 1
end

local totalWorkers = tonumber(os.getenv("TEST_WORKERS")) or getCpuCount()
local workerId = os.getenv("TEST_WORKER")  -- set when we are a worker

-- Main process: spawn N workers in parallel when TEST_WORKERS > 1 and we are not already a worker
if totalWorkers > 1 and workerId == nil then
  local scriptFull = projectRoot .. "bin" .. sep .. "test.lua"
  local tmpBase = os.getenv("TEMP") or os.getenv("TMP") or (isWindows and "." or "/tmp")
  local resultDir = tmpBase .. sep .. "deltachess_test_" .. tostring(math.random(100000, 999999))
  if isWindows then
    os.execute("mkdir " .. escapeshellarg(resultDir) .. " 2>nul")
  else
    os.execute("mkdir -p " .. escapeshellarg(resultDir) .. " 2>/dev/null")
  end
  print("=====================================")
  print("  DeltaChess Test Suite (parallel, " .. totalWorkers .. " workers)")
  print("=====================================")
  print("")

  -- Helper script: create pid file, run worker script, remove pid file when done
  if isWindows then
    local helperBat = resultDir .. sep .. "run_with_pid.bat"
    local helperContent = "@echo off\r\n"
      .. "if \"%~1\"==\"\" exit /b 1\r\n"
      .. "if \"%~2\"==\"\" exit /b 1\r\n"
      .. "echo. > \"%~1\"\r\n"
      .. "call \"%~2\"\r\n"
      .. "set EXIT=%ERRORLEVEL%\r\n"
      .. "del /q \"%~1\" 2>nul\r\n"
      .. "exit /b %EXIT%\r\n"
    local h = io.open(helperBat, "wb")
    if h then h:write(helperContent); h:close() end
  else
    local helperSh = resultDir .. sep .. "run_with_pid.sh"
    local helperContent = "#!/bin/sh\n"
      .. "PIDFILE=\"$1\"\n"
      .. "SCRIPT=\"$2\"\n"
      .. "[ -z \"$PIDFILE\" ] || [ -z \"$SCRIPT\" ] && exit 1\n"
      .. "touch \"$PIDFILE\"\n"
      .. "\"$SCRIPT\"\n"
      .. "EXIT=$?\n"
      .. "rm -f \"$PIDFILE\"\n"
      .. "exit $EXIT\n"
    local h = io.open(helperSh, "wb")
    if h then h:write(helperContent); h:close() end
    os.execute("chmod +x " .. escapeshellarg(resultDir .. sep .. "run_with_pid.sh") .. " 2>/dev/null")
  end

  -- Start each worker via helper (creates pid file, runs worker, removes pid file on exit)
  for i = 0, totalWorkers - 1 do
    local pidFile = resultDir .. sep .. "pid_" .. i
    local cmd
    if isWindows then
      local batPath = resultDir .. sep .. "run_" .. i .. ".bat"
      local function batStr(s)
        return (tostring(s):gsub("%%", "%%%%"))
      end
      local line = '@echo off\r\n'
        .. 'set "TEST_RESULT_DIR=' .. batStr(resultDir) .. '"\r\n'
        .. 'set "TEST_WORKER=' .. i .. '"\r\n'
        .. 'set "TEST_WORKERS=' .. totalWorkers .. '"\r\n'
      if luaBin ~= "lua" then
        line = line .. 'set "LUA_BIN=' .. batStr(luaBin) .. '"\r\n'
      end
      if #onlySlugs > 0 then
        line = line .. 'set "TEST_ONLY_SLUGS=' .. batStr(table.concat(onlySlugs, ",")) .. '"\r\n'
      end
      if #ignoreSlugs > 0 then
        line = line .. 'set "TEST_IGNORE_SLUGS=' .. batStr(table.concat(ignoreSlugs, ",")) .. '"\r\n'
      end
      line = line .. 'cd /d "' .. batStr(projectRoot:gsub("/", sep)) .. '"\r\n'
        .. '"' .. batStr(luaBin:gsub("/", sep)) .. '" "' .. batStr(scriptFull:gsub("/", sep)) .. '"\r\n'
      local f = io.open(batPath, "wb")
      if f then
        f:write(line)
        f:close()
        local helperPath = resultDir .. sep .. "run_with_pid.bat"
        cmd = 'start /b cmd /c "' .. escapeshellargCmdInner(helperPath) .. ' ' .. escapeshellargCmdInner(pidFile) .. ' ' .. escapeshellargCmdInner(batPath) .. '"'
      else
        cmd = 'echo Failed to write ' .. batPath .. ' & exit 1'
      end
      os.execute(cmd)
    else
      local shPath = resultDir .. sep .. "run_" .. i .. ".sh"
      local shContent = "#!/bin/sh\n"
        .. "export TEST_RESULT_DIR=\"" .. forShDoubleQuotedValue(resultDir) .. "\"\n"
        .. "export TEST_WORKER=" .. i .. "\n"
        .. "export TEST_WORKERS=" .. totalWorkers .. "\n"
      if luaBin ~= "lua" then
        shContent = shContent .. "export LUA_BIN=\"" .. forShDoubleQuotedValue(luaBin) .. "\"\n"
      end
      if #onlySlugs > 0 then
        shContent = shContent .. "export TEST_ONLY_SLUGS=\"" .. forShDoubleQuotedValue(table.concat(onlySlugs, ",")) .. "\"\n"
      end
      if #ignoreSlugs > 0 then
        shContent = shContent .. "export TEST_IGNORE_SLUGS=\"" .. forShDoubleQuotedValue(table.concat(ignoreSlugs, ",")) .. "\"\n"
      end
      shContent = shContent .. "cd \"" .. forShDoubleQuotedValue(projectRoot) .. "\" && exec \""
        .. forShDoubleQuotedValue(luaBin) .. "\" \"" .. forShDoubleQuotedValue(scriptFull) .. "\"\n"
      local f = io.open(shPath, "wb")
      if f then
        f:write(shContent)
        f:close()
        os.execute("chmod +x " .. escapeshellarg(shPath) .. " 2>/dev/null")
        local helperSh = resultDir .. sep .. "run_with_pid.sh"
        cmd = "(" .. escapeshellarg(helperSh) .. " " .. escapeshellarg(pidFile) .. " " .. escapeshellarg(shPath) .. ") &"
      else
        cmd = "echo Failed to write " .. shPath .. " ; exit 1"
      end
      os.execute(cmd)
    end
  end

  -- Wait until all pid files are removed (workers finished)
  local function anyPidFileExists()
    for i = 0, totalWorkers - 1 do
      local f = io.open(resultDir .. sep .. "pid_" .. i, "r")
      if f then f:close(); return true end
    end
    return false
  end
  while anyPidFileExists() do
    if isWindows then
      os.execute("ping -n 2 127.0.0.1 > nul 2>&1")
    else
      os.execute("sleep 1 2>/dev/null")
    end
  end

  -- Collect stats (passed, failed) from each worker and print combined summary
  local totalPassed, totalFailed = 0, 0
  for i = 0, totalWorkers - 1 do
    local f = io.open(resultDir .. sep .. "stats_" .. i, "r")
    if f then
      local content = f:read("a") or ""
      f:close()
      local p, q = content:match("^(%d+)\r?\n(%d+)")
      totalPassed = totalPassed + (tonumber(p) or 0)
      totalFailed = totalFailed + (tonumber(q) or 0)
    end
  end
  if isWindows then
    os.execute("rmdir /s /q " .. escapeshellarg(resultDir) .. " 2>nul")
  else
    os.execute("rm -rf " .. escapeshellarg(resultDir) .. " 2>/dev/null")
  end
  print("")
  print("=====================================")
  print(string.format("Tests: %d passed, %d failed", totalPassed, totalFailed))
  print("=====================================")
  os.exit(totalFailed > 0 and 1 or 0)
end

-- Single process or worker: run tests (workers run only their slice)
print("=====================================")
print("  DeltaChess Test Suite" .. (workerId and (" (worker " .. (workerId + 1) .. "/" .. totalWorkers .. ")") or ""))
print("=====================================")
print("")

for _, dir in ipairs(testDirs) do
  package.path = dir .. "/?.lua;" .. package.path
  local dirName = dir:match("([^/\\]+)$") or dir
  if not Test.parallelMode then
    print("=== " .. dirName:upper() .. " TESTS ===")
    print("")
  end
  Test.runAll(dir)
end

if Test.parallelMode then
  Test.runWorkerSlice(Test.workerId, Test.totalWorkers)
  -- Workers do not print summary; they write stats for the main to aggregate
end

-- In parallel worker mode, write stats (passed, failed) for main to aggregate
local resultDir = os.getenv("TEST_RESULT_DIR")
local wid = os.getenv("TEST_WORKER")
if resultDir and wid ~= nil and wid ~= "" then
  local statsPath = resultDir .. sep .. "stats_" .. wid
  local f = io.open(statsPath, "w")
  if f then
    f:write(tostring(Test.passed), "\n", tostring(Test.failed))
    f:close()
  end
end

Test.exit()
